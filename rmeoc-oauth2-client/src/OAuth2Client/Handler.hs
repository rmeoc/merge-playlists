{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}

module OAuth2Client.Handler (withAccessToken, withAccessToken_) where

import Control.Lens                 ((&), (.~), (?~), (^.))
import Control.Monad                (when)
import Control.Monad.Reader         (MonadReader, ask)
import Crypto.Nonce                 (nonce128url)
import Data.Aeson                   (FromJSON, genericParseJSON, parseJSON)
import Data.Aeson.Casing            (aesonPrefix, snakeCase)
import Data.Either.Validation       (Validation(..), validationToEither)
import Data.Functor.Alt             ((<!>))
import Data.Maybe                   (isJust)
import Data.List.NonEmpty           (NonEmpty(..))
import Data.Text                    (Text)
import Data.Text.Encoding           (decodeUtf8, encodeUtf8)
import Data.Typeable                (Typeable)
import GHC.Generics                 (Generic)
import Network.HTTP.Client          (HttpException(..), HttpExceptionContent(..))
import Network.Wreq                 (FormParam(..), responseBody)
import OAuth2Client.Foundation      (OAuth2ClientConf(..), OAuth2ClientSubsite(..), Route(..), SessionKey(..), Url,
                                    resourcesOAuth2ClientSubsite)
import UnliftIO                     (MonadUnliftIO)
import UnliftIO.Exception           (Exception, throwIO, try, tryJust)
import URI.ByteString               (queryL, queryPairsL, serializeURIRef')
import Yesod.Core                   (HandlerSite, Html, MonadHandler, SubHandlerSite, Yesod, YesodSubDispatch,
                                     defaultLayout, deleteSession, getRouteToParent,
                                     getSubYesod, getUrlRender, invalidArgs, liftHandler, liftIO,
                                     lookupGetParam, lookupSession, mkYesodSubDispatch, permissionDenied,
                                     redirect, redirectUltDest, setSession, setUltDestCurrent, whamlet, yesodSubDispatch)

import qualified Control.Newtype as N
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as WS

data AuthorizationException = AuthorizationException deriving (Show, Typeable)

instance Exception AuthorizationException
  
urlToString :: Url -> String
urlToString = T.unpack . decodeUtf8 . serializeURIRef' . N.unpack 

withAccessToken :: (MonadHandler m, MonadUnliftIO m, MonadReader WS.Session m) => OAuth2ClientSubsite -> (Route OAuth2ClientSubsite -> Route (HandlerSite m)) -> (Text -> m a) -> m a
withAccessToken sy toParent action = do
    redirectToAuthorizeOnFail sy toParent $ withAccessToken_ sy action

withAccessToken_ :: (MonadHandler m, MonadUnliftIO m, MonadReader WS.Session m) => OAuth2ClientSubsite -> (Text -> m a) -> m a
withAccessToken_ sy action = do
    refreshTokenAndRetryOnFail sy $ do
        accessToken <- maybe (throwIO AuthorizationException) return =<< lookupSession (ocsSessionKey sy SessionKeyAccessToken)
        translateToAuthorizationException [401] $ 
            action accessToken

getOAuthCallbackUri :: (MonadHandler m) => (Route OAuth2ClientSubsite -> Route (HandlerSite m)) -> m Text
getOAuthCallbackUri toParent = do
    urlRender <- getUrlRender
    return $ urlRender $ toParent CallbackR

redirectToAuthorizeOnFail :: (MonadHandler m, MonadUnliftIO m) => OAuth2ClientSubsite -> (Route OAuth2ClientSubsite -> Route (HandlerSite m)) -> m a -> m a
redirectToAuthorizeOnFail sy toParent action = do
        
        x <- try action
        
        let sessionKeyRetrying = ocsSessionKey sy SessionKeyRetryingWithNewAccessToken
        retrying <- fmap isJust $ lookupSession sessionKeyRetrying
        deleteSession sessionKeyRetrying
        case x of
            Left AuthorizationException -> do
                
                when retrying $ throwIO AuthorizationException

                redirectUri <- getOAuthCallbackUri toParent
                state <- nonce128url $ ocsNonceGenerator sy
                setSession (ocsSessionKey sy SessionKeyState) (decodeUtf8 state)
                setSession sessionKeyRetrying ""
                setUltDestCurrent
                redirect $ decodeUtf8 $ serializeURIRef' $ authorizeUrl' sy redirectUri state

            Right playlists -> return playlists
    where
        authorizeUrl' (OAuth2ClientSubsite {..}) redirectUri state 
                = authorizeUrl & queryL . queryPairsL .~
                    [ ("client_id", encodeUtf8 $ occClientId ocsConfig)
                    , ("response_type", "code")
                    , ("redirect_uri", encodeUtf8 $ redirectUri)
                    , ("state", state)
                    , ("scope", encodeUtf8 $ T.intercalate " " $ occScopes ocsConfig)
                    ]
            where
                authorizeUrl = N.unpack $ occAuthorizeUrl ocsConfig
    
refreshTokenAndRetryOnFail :: (MonadHandler m, MonadUnliftIO m, MonadReader WS.Session m) => OAuth2ClientSubsite -> m a -> m a
refreshTokenAndRetryOnFail sy action = do

        x <- try action

        case x of
            Left AuthorizationException -> do
                
                maybeRefreshToken <- lookupSession $ ocsSessionKey sy SessionKeyRefreshToken

                maybe
                    (throwIO AuthorizationException)
                    (\refreshToken -> do
                        
                        let tokenUrl = occTokenUrl $ ocsConfig sy
                        let clientId = encodeUtf8 $ occClientId $ ocsConfig sy
                        let clientSecret = encodeUtf8 $ occClientSecret $ ocsConfig sy
                        wreqSession <- ask
                        accessTokenResp <-
                            translateToAuthorizationException [400, 401] $
                                liftIO $
                                    W.asJSON =<< WS.postWith
                                        (W.defaults & W.auth ?~ W.basicAuth clientId clientSecret)
                                        wreqSession
                                        (urlToString tokenUrl)
                                        [ "grant_type" := ("refresh_token" :: Text)
                                        , "refresh_token" := refreshToken
                                        ]
                        handleAccessTokenResponse sy $ accessTokenResp ^. responseBody
                        action)
                    maybeRefreshToken

            Right playlists -> return playlists

translateToAuthorizationException :: (MonadUnliftIO m, Foldable t) => t Int -> m a -> m a
translateToAuthorizationException statusCodes x = tryJust filterStatusCodeException x >>= either (const $ throwIO AuthorizationException) return
    where
        filterStatusCodeException :: HttpException -> Maybe HttpException
        filterStatusCodeException e = case e of
            HttpExceptionRequest _ (StatusCodeException response _) ->
                if elem (response ^. W.responseStatus . W.statusCode) statusCodes then Just e else Nothing
            _ -> Nothing

type ErrorList = NonEmpty Text

data UnparsedAuthorizationResponse
    = UnparsedAuthorizationResponse
        { uarStateGet :: Validation ErrorList Text
        , uarStateSession :: Validation ErrorList Text
        , uarError :: Validation ErrorList Text
        , uarCode :: Validation ErrorList Text
        }

data AuthorizationResponse
    = AuthorizationResponseSuccess Text
    | AuthorizationResponseError Text

data AccessTokenResponse = AccessTokenResponse
    { atrAccessToken :: Text
    , atrTokenType :: Text
    , atrScope :: Maybe Text
    , atrExpiresIn :: Maybe Integer
    , atrRefreshToken :: Maybe Text
    } deriving (Generic, Show)

instance FromJSON AccessTokenResponse where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

failWithMessage :: Text -> Either ErrorList a
failWithMessage message = Left $ message :| []

withErrorMessage :: Text -> Maybe a -> Validation ErrorList a
withErrorMessage message = maybe (Failure (message :| [])) Success

parseAuthorizationResponse :: UnparsedAuthorizationResponse -> Either ErrorList AuthorizationResponse
parseAuthorizationResponse UnparsedAuthorizationResponse {..} = do
    equalStates <- validationToEither ((==) <$> uarStateGet <*> uarStateSession)
    when (not equalStates) $ failWithMessage "The state from the GET parameter does not match the state from the session"
    validationToEither
        (   AuthorizationResponseError <$> uarError
        <!> AuthorizationResponseSuccess <$> uarCode
        )

instance Yesod master => YesodSubDispatch OAuth2ClientSubsite master where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesOAuth2ClientSubsite)

getDummyR :: (MonadHandler m, Yesod (HandlerSite m)) => m Html
getDummyR = do
    liftHandler $
        defaultLayout $
            [whamlet|
                <h1>Dummy Page
            |]
        
getCallbackR :: (MonadHandler m, SubHandlerSite m ~ OAuth2ClientSubsite) => m ()
getCallbackR = do
    sy <- getSubYesod
    let sessionKeyState = ocsSessionKey sy SessionKeyState
    uarStateGet <- withErrorMessage "GET request doesn't specify a state" <$> lookupGetParam "state"
    uarStateSession <- withErrorMessage "Session doesn't specify a state" <$> lookupSession sessionKeyState
    uarError <- withErrorMessage "GET request doesn't specify error" <$> lookupGetParam "error"
    uarCode <- withErrorMessage "GET request doesn't specify code" <$> lookupGetParam "code"
    deleteSession sessionKeyState
    authorizationResponse <- either (invalidArgs . NE.toList) pure $ parseAuthorizationResponse (UnparsedAuthorizationResponse {..})
    case authorizationResponse of
        AuthorizationResponseError err -> do
            deleteSession $ ocsSessionKey sy SessionKeyRetryingWithNewAccessToken
            permissionDenied err

        AuthorizationResponseSuccess code -> do
            wreqSession <- liftIO WS.newSession

            let tokenUrl = occTokenUrl $ ocsConfig sy
            let clientId = encodeUtf8 $ occClientId $ ocsConfig sy
            let clientSecret = encodeUtf8 $ occClientSecret $ ocsConfig sy
            toParent <- getRouteToParent
            redirectUri <- getOAuthCallbackUri toParent
            accessTokenResp <- liftIO $
                W.asJSON =<< WS.postWith
                    (W.defaults & W.auth ?~ W.basicAuth clientId clientSecret)
                    wreqSession
                    (urlToString tokenUrl)
                    [ "grant_type" := ("authorization_code" :: Text)
                    , "code" := code
                    , "redirect_uri" := redirectUri
                    ]

            let resp = accessTokenResp ^. responseBody

            handleAccessTokenResponse sy resp

            redirectUltDest (toParent DummyR)

handleAccessTokenResponse :: MonadHandler m => OAuth2ClientSubsite -> AccessTokenResponse -> m ()
handleAccessTokenResponse sy resp = do
    setSession (ocsSessionKey sy SessionKeyAccessToken) $ atrAccessToken resp
    maybe (return ()) (setSession $ ocsSessionKey sy SessionKeyRefreshToken) $ atrRefreshToken resp
