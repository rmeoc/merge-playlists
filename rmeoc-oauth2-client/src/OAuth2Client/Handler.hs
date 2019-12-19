{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}

module OAuth2Client.Handler (withAccessToken, withAccessToken_) where

import Control.Lens                 ((&), (.~))
import Control.Monad                (when)
import Control.Monad.Except         (throwError)
import Crypto.Nonce                 (nonce128url)
import Data.ByteString              (ByteString)
import Data.Either.Validation       (Validation(..), eitherToValidation, validationToEither)
import Data.Functor.Alt             ((<!>))
import Data.Maybe                   (isJust)
import Data.Text                    (Text)
import Data.Text.Encoding           (decodeUtf8, encodeUtf8)
import Data.Typeable                (Typeable)
import Network.HTTP.Client          (HttpException(..), HttpExceptionContent(..), responseStatus)
import Network.HTTP.Types.Status    (statusCode)
import Network.OAuth.OAuth2         (ExchangeToken(..), OAuth2(..), OAuth2Token(..), RefreshToken(..),
                                     atoken, authorizationUrl, fetchAccessToken, refreshAccessToken, rtoken)
import OAuth2Client.Foundation      (OAuth2ClientConf(..), OAuth2ClientSubsite(..), Route(..), SessionKey(..), Url(..),
                                    resourcesOAuth2ClientSubsite)
import UnliftIO                     (MonadUnliftIO)
import UnliftIO.Exception           (Exception, throwIO, throwString, try, tryJust)
import URI.ByteString               (parseURI, queryL, queryPairsL, serializeURIRef', strictURIParserOptions)
import Yesod.Core                   (HandlerSite, Html, MonadHandler, SubHandlerSite,
                                     Yesod, YesodSubDispatch, defaultLayout, deleteSession, getRouteToParent,
                                     getSubYesod, getUrlRender, liftHandler, liftIO, logError,
                                     lookupGetParam, lookupSession, mkYesodSubDispatch, permissionDenied,
                                     redirect, redirectUltDest, setSession, setUltDestCurrent, whamlet, yesodSubDispatch)

import qualified Control.Newtype as N
import qualified Data.Text as T

data AuthorizationException = AuthorizationException deriving (Show, Typeable)

instance Exception AuthorizationException

oauth2Settings :: OAuth2ClientConf -> Maybe Url -> ByteString -> OAuth2
oauth2Settings OAuth2ClientConf { occAuthorizeUrl, occClientId, occClientSecret, occScopes, occTokenUrl } callbackUrl state =
    OAuth2
        { oauthClientId = occClientId
        , oauthClientSecret = occClientSecret
        , oauthCallback = N.unpack <$> callbackUrl
        , oauthOAuthorizeEndpoint = 
            N.unpack occAuthorizeUrl & queryL . queryPairsL .~
                [ ("state", state)
                , ("scope", encodeUtf8 $ T.intercalate " " occScopes)
                ]
        , oauthAccessTokenEndpoint = N.unpack occTokenUrl
        }

oauth2SettingsNoState :: OAuth2ClientConf -> Maybe Url -> OAuth2
oauth2SettingsNoState OAuth2ClientConf { occClientId, occClientSecret, occTokenUrl } callbackUrl =
    OAuth2
        { oauthClientId = occClientId
        , oauthClientSecret = occClientSecret
        , oauthCallback = N.unpack <$> callbackUrl
        , oauthOAuthorizeEndpoint = error "Can't touch this (oauthOAuthorizeEndpoint)"
        , oauthAccessTokenEndpoint = N.unpack occTokenUrl
        }

withAccessToken :: (MonadHandler m, MonadUnliftIO m) => OAuth2ClientSubsite -> (Route OAuth2ClientSubsite -> Route (HandlerSite m)) -> (Text -> m a) -> m a
withAccessToken sy toParent action = do
    redirectToAuthorizeOnFail sy toParent $ withAccessToken_ sy action

withAccessToken_ :: (MonadHandler m, MonadUnliftIO m) => OAuth2ClientSubsite -> (Text -> m a) -> m a
withAccessToken_ sy action = do
    refreshTokenAndRetryOnFail sy $ do
        accessToken <- maybe (throwIO AuthorizationException) return =<< lookupSession (ocsSessionKey sy SessionKeyAccessToken)
        translateToAuthorizationException [401] $ 
            action accessToken

getOAuthCallbackUri :: (MonadHandler m) => (Route OAuth2ClientSubsite -> Route (HandlerSite m)) -> m Url
getOAuthCallbackUri toParent = do
    urlRender <- getUrlRender
    let urlText = urlRender $ toParent CallbackR
    either (const $ throwString $ "Invalid callback uri: " <> T.unpack urlText) (pure . Url) $
        parseURI strictURIParserOptions (encodeUtf8 urlText)

redirectToAuthorizeOnFail :: (MonadHandler m, MonadUnliftIO m) => OAuth2ClientSubsite -> (Route OAuth2ClientSubsite -> Route (HandlerSite m)) -> m a -> m a
redirectToAuthorizeOnFail sy toParent action = do
        
        x <- try action
        
        let sessionKeyRetrying = ocsSessionKey sy SessionKeyRetryingWithNewAccessToken
        retrying <- fmap isJust $ lookupSession sessionKeyRetrying
        deleteSession sessionKeyRetrying
        case x of
            Left AuthorizationException -> do
                
                when retrying $ throwIO AuthorizationException

                state <- nonce128url $ ocsNonceGenerator sy
                redirectUri <- getOAuthCallbackUri toParent

                setSession (ocsSessionKey sy SessionKeyState) (decodeUtf8 state)
                setSession sessionKeyRetrying ""
                setUltDestCurrent
                
                redirect $
                    decodeUtf8 $
                        serializeURIRef' $
                            authorizationUrl $
                                oauth2Settings (ocsConfig sy) (Just redirectUri) state

            Right actionResult -> return actionResult
    
refreshTokenAndRetryOnFail :: (MonadHandler m, MonadUnliftIO m) => OAuth2ClientSubsite -> m a -> m a
refreshTokenAndRetryOnFail sy action = do

        x <- try action

        case x of
            Left AuthorizationException -> do
                
                maybeRefreshToken <- lookupSession $ ocsSessionKey sy SessionKeyRefreshToken

                maybe
                    (throwIO AuthorizationException)
                    (\refreshToken -> do
                        accessTokenResp <-
                            liftIO $
                                refreshAccessToken
                                    (ocsHttpManager sy)
                                    (oauth2SettingsNoState (ocsConfig sy) Nothing)
                                    (RefreshToken refreshToken)
                        tokens <- either (const $ throwIO AuthorizationException) return accessTokenResp
                        storeTokens sy tokens
                        action)
                    maybeRefreshToken

            Right playlists -> return playlists

translateToAuthorizationException :: (MonadUnliftIO m, Foldable t) => t Int -> m a -> m a
translateToAuthorizationException statusCodes x = tryJust filterStatusCodeException x >>= either (const $ throwIO AuthorizationException) return
    where
        filterStatusCodeException :: HttpException -> Maybe HttpException
        filterStatusCodeException e = case e of
            HttpExceptionRequest _ (StatusCodeException response _) ->
                if elem (statusCode $ responseStatus response) statusCodes then Just e else Nothing
            _ -> Nothing

data AuthorizationResponse
    = AuthorizationResponseSuccess Text
    | AuthorizationResponseError Text

instance Yesod master => YesodSubDispatch OAuth2ClientSubsite master where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesOAuth2ClientSubsite)

getDummyR :: (MonadHandler m, Yesod (HandlerSite m)) => m Html
getDummyR = do
    liftHandler $
        defaultLayout $
            [whamlet|
                <h1>Dummy Page
            |]

onAuthorizationFailed :: (MonadHandler m, SubHandlerSite m ~ OAuth2ClientSubsite) => m a
onAuthorizationFailed = do
    sy <- getSubYesod
    deleteSession $ ocsSessionKey sy SessionKeyRetryingWithNewAccessToken
    permissionDenied "Forbidden" 
    
getCallbackR :: (MonadHandler m, SubHandlerSite m ~ OAuth2ClientSubsite) => m ()
getCallbackR = do
    sy <- getSubYesod
    let sessionKeyState = ocsSessionKey sy SessionKeyState

    let withErrorMessage :: Text -> Maybe a -> Validation [Text] a
        withErrorMessage message = maybe (Failure [message]) pure

    stateReceived <- withErrorMessage "GET request doesn't specify a state" <$> lookupGetParam "state"
    stateStored <- withErrorMessage "Session doesn't specify a state" <$> lookupSession sessionKeyState
    errorParam <- withErrorMessage "GET request doesn't specify error" <$> lookupGetParam "error"
    codeParam <- withErrorMessage "GET request doesn't specify code" <$> lookupGetParam "code"
    deleteSession sessionKeyState

    let checkState =
            eitherToValidation $ do
                equalStates <- validationToEither $ (==) <$> stateReceived <*> stateStored
                when (not equalStates) $ throwError ["The state from the GET parameter does not match the state from the session"]

    let validationResult = validationToEither $
            checkState *> (AuthorizationResponseError <$> errorParam <!> AuthorizationResponseSuccess <$> codeParam)

    authorizationResponse <-
        either
            (\errs -> do
                $(logError) $ "Failed to parse authorization response: " <> T.pack (show errs)
                onAuthorizationFailed)
            pure
            validationResult

    case authorizationResponse of
        AuthorizationResponseError err -> do
            $(logError) $ "Authorization failed: " <> err
            onAuthorizationFailed

        AuthorizationResponseSuccess code -> do
            toParent <- getRouteToParent
            redirectUri <- getOAuthCallbackUri toParent
            accessTokenResp <-
                liftIO $
                    fetchAccessToken
                        (ocsHttpManager sy)
                        (oauth2SettingsNoState (ocsConfig sy) $ Just redirectUri)
                        (ExchangeToken code)
            tokens <- either
                (\err -> do
                    $(logError) $ "Failed to fetch access token: " <> T.pack (show err)
                    onAuthorizationFailed)
                return
                accessTokenResp
            storeTokens sy tokens
            redirectUltDest (toParent DummyR)

storeTokens :: MonadHandler m => OAuth2ClientSubsite -> OAuth2Token -> m ()
storeTokens sy OAuth2Token { accessToken, refreshToken } = do
    setSession (ocsSessionKey sy SessionKeyAccessToken) $ atoken accessToken
    maybe (return ()) (setSession $ ocsSessionKey sy SessionKeyRefreshToken) $ rtoken <$> refreshToken
