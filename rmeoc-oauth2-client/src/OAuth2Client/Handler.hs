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

module OAuth2Client.Handler (getAccessToken, handleCallback, redirectToAuthorizationPage) where

import Control.Lens                 ((&), (.~))
import Control.Monad                (liftM3, when)
import Control.Monad.Except         (throwError)
import Crypto.Nonce                 (nonce128url)
import Data.ByteString              (ByteString)
import Data.Either.Validation       (Validation(..), eitherToValidation, validationToEither)
import Data.Functor.Alt             ((<!>))
import Data.Maybe                   (fromMaybe)
import Data.Text                    (Text)
import Data.Text.Encoding           (decodeUtf8, encodeUtf8)
import Data.Time.Clock.POSIX        (getPOSIXTime)
import Data.Typeable                (Typeable)
import Network.OAuth.OAuth2         (AccessToken(..), ExchangeToken(..), OAuth2(..), OAuth2Token(..), RefreshToken(..),
                                     atoken, authorizationUrl, fetchAccessToken, refreshAccessToken, rtoken)
import OAuth2Client.Context         (OAuth2ClientConf(..), OAuth2ClientContext(..), SessionKey(..), Url(..))
import UnliftIO                     (MonadIO, MonadUnliftIO)
import UnliftIO.Exception           (Exception, throwIO, throwString)
import URI.ByteString               (parseURI, queryL, queryPairsL, serializeURIRef', strictURIParserOptions)
import Yesod.Core                   (HandlerSite, MonadHandler, Route, deleteSession, getUrlRender, liftIO,
                                     logError, lookupGetParam, lookupSession, lookupSessionBS, permissionDenied,
                                     redirect, setSession, setSessionBS)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary as B
import qualified Control.Newtype as N
import qualified Data.Text as T


data AuthorizationException = AuthorizationException deriving (Show, Typeable)

instance Exception AuthorizationException


data AccessTokenInfo = AccessTokenInfo { tokAccessToken :: AccessToken, tokReceivedAtSeconds :: Int, tokExpiresInSeconds :: Maybe Int }

instance B.Binary AccessTokenInfo where
    put AccessTokenInfo { tokAccessToken, tokReceivedAtSeconds, tokExpiresInSeconds }
        = B.put (atoken tokAccessToken) <> B.put tokReceivedAtSeconds <> B.put tokExpiresInSeconds
    get = liftM3 AccessTokenInfo (AccessToken <$> B.get) B.get B.get


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

redirectToAuthorizationPage :: (MonadHandler m) => OAuth2ClientContext -> Route (HandlerSite m) -> m a
redirectToAuthorizationPage ctx callbackR = do
    state <- nonce128url $ ocxNonceGenerator ctx
    redirectUri <- getOAuthCallbackUri callbackR
    setSession (ocxSessionKey ctx SessionKeyState) (decodeUtf8 state)
    redirect $
        decodeUtf8 $
            serializeURIRef' $
                authorizationUrl $
                    oauth2Settings (ocxConfig ctx) (Just redirectUri) state

getAccessToken :: (MonadHandler m, MonadUnliftIO m) => OAuth2ClientContext -> m Text
getAccessToken ctx = do
    refreshNeeded <- isAccessTokenRefreshNeeded ctx
    when refreshNeeded $ refreshAccessToken_ ctx
    mtoken <- lookupAccessToken ctx
    maybe (throwIO AuthorizationException) (pure . atoken) mtoken

getOAuthCallbackUri :: (MonadHandler m) => Route (HandlerSite m) -> m Url
getOAuthCallbackUri callbackR = do
    urlRender <- getUrlRender
    let urlText = urlRender callbackR
    either (const $ throwString $ "Invalid callback uri: " <> T.unpack urlText) (pure . Url) $
        parseURI strictURIParserOptions (encodeUtf8 urlText)

refreshAccessToken_ :: (MonadHandler m, MonadUnliftIO m) => OAuth2ClientContext -> m ()
refreshAccessToken_ ctx = do
    maybeRefreshToken <- lookupSession $ ocxSessionKey ctx SessionKeyRefreshToken
    maybe
        (throwIO AuthorizationException)
        (\refreshToken -> do
            accessTokenResp <-
                liftIO $
                    refreshAccessToken
                        (ocxHttpManager ctx)
                        (oauth2SettingsNoState (ocxConfig ctx) Nothing)
                        (RefreshToken refreshToken)
            tokens <- either (const $ throwIO AuthorizationException) return accessTokenResp
            storeTokens ctx tokens)
        maybeRefreshToken

data AuthorizationResponse
    = AuthorizationResponseSuccess Text
    | AuthorizationResponseError Text

handleCallback :: (MonadHandler m) => OAuth2ClientContext -> Route (HandlerSite m) -> m ()
handleCallback ctx callbackR = do
    let sessionKeyState = ocxSessionKey ctx SessionKeyState

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
            redirectUri <- getOAuthCallbackUri callbackR
            accessTokenResp <-
                liftIO $
                    fetchAccessToken
                        (ocxHttpManager ctx)
                        (oauth2SettingsNoState (ocxConfig ctx) $ Just redirectUri)
                        (ExchangeToken code)
            tokens <- either
                (\err -> do
                    $(logError) $ "Failed to fetch access token: " <> T.pack (show err)
                    onAuthorizationFailed)
                return
                accessTokenResp
            storeTokens ctx tokens
  where
    onAuthorizationFailed :: (MonadHandler m) => m a
    onAuthorizationFailed = permissionDenied "Forbidden" 

storeTokens :: MonadHandler m => OAuth2ClientContext -> OAuth2Token -> m ()
storeTokens ctx token = do
    accessTokenInfo <- createAccessTokenInfo token
    setSessionBS (ocxSessionKey ctx SessionKeyAccessTokenInfo) $ BSL.toStrict $ B.encode accessTokenInfo
    maybe (return ()) (setSession $ ocxSessionKey ctx SessionKeyRefreshToken) $ rtoken <$> refreshToken token

createAccessTokenInfo :: (MonadIO m) => OAuth2Token -> m AccessTokenInfo
createAccessTokenInfo OAuth2Token { accessToken, expiresIn } = do
    receivedAt <- round <$> liftIO getPOSIXTime
    pure $ AccessTokenInfo accessToken receivedAt expiresIn

lookupAccessTokenInfo :: MonadHandler m => OAuth2ClientContext -> m (Maybe AccessTokenInfo)
lookupAccessTokenInfo ctx = do
    mbs <- lookupSessionBS (ocxSessionKey ctx SessionKeyAccessTokenInfo)
    pure $ fmap (B.decode . BSL.fromStrict) mbs
    
lookupAccessToken :: MonadHandler m => OAuth2ClientContext -> m (Maybe AccessToken)
lookupAccessToken ctx = fmap tokAccessToken <$> lookupAccessTokenInfo ctx

isAccessTokenRefreshNeeded :: MonadHandler m => OAuth2ClientContext -> m Bool
isAccessTokenRefreshNeeded ctx = do
    maccessTokenInfo <- lookupAccessTokenInfo ctx
    maybe
        (pure True)
        (\AccessTokenInfo { tokReceivedAtSeconds, tokExpiresInSeconds } -> do
            let cfg = ocxConfig ctx
            let expiresInSeconds = fromMaybe (occTokenExpiresInDefault cfg) tokExpiresInSeconds
            let refreshAt = tokReceivedAtSeconds + expiresInSeconds - occRefreshTokenWhenExpiresIn cfg
            time <- round <$> liftIO getPOSIXTime
            pure $ time >= refreshAt)
        maccessTokenInfo
