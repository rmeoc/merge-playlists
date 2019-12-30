{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module OAuth2Client.Context
    ( OAuth2ClientConf(..)
    , OAuth2ClientContext(..)
    , SessionKey(..)
    , Url(..)
    , deleteOAuth2ClientContext
    , initOAuth2ClientContext
    ) where

import Control.Monad.IO.Class   (MonadIO)
import Data.Aeson               (FromJSON, genericParseJSON, parseJSON, withText)
import Data.Aeson.Casing        (aesonPrefix, trainCase)
import Data.Either              (either)
import Data.Text                (Text)
import Data.Text.Encoding       (encodeUtf8)
import GHC.Generics             (Generic)
import Network.HTTP.Client      (Manager)
import URI.ByteString           (Absolute, URIRef, parseURI, strictURIParserOptions)
import Yesod.Core               (Yesod, Route, mkYesodSubData, parseRoutesFile, renderRoute)

import qualified Crypto.Nonce as N
import qualified Control.Newtype as NT


newtype Url = Url (URIRef Absolute)

instance NT.Newtype Url (URIRef Absolute) where
    pack = Url
    unpack (Url a) = a

instance FromJSON Url where
    parseJSON =  withText "URL" (\txt -> do
        either (\err -> fail $ "parsing URL failed, " <> show err) (return . Url) $
            parseURI strictURIParserOptions (encodeUtf8 txt))

data OAuth2ClientConf = OAuth2ClientConf
    { occAuthorizeUrl :: Url
    , occClientId :: Text
    , occClientSecret :: Text
    , occTokenUrl :: Url
    , occScopes :: [Text]
    } deriving (Generic)

instance FromJSON OAuth2ClientConf where
    parseJSON = genericParseJSON $ aesonPrefix trainCase

data SessionKey
    = SessionKeyAccessToken
    | SessionKeyRefreshToken
    | SessionKeyState

data OAuth2ClientContext = OAuth2ClientContext
    { ocxSessionKey :: SessionKey -> Text
    , ocxConfig :: OAuth2ClientConf
    , ocxNonceGenerator :: N.Generator
    , ocxHttpManager :: Manager
    }

initOAuth2ClientContext :: MonadIO m => (SessionKey -> Text) -> OAuth2ClientConf -> Manager -> m OAuth2ClientContext
initOAuth2ClientContext translateSessionKey conf manager = do
    nonceGen <- N.new
    return OAuth2ClientContext
        { ocxSessionKey = translateSessionKey
        , ocxConfig = conf
        , ocxNonceGenerator = nonceGen
        , ocxHttpManager = manager
        }

deleteOAuth2ClientContext :: MonadIO m => OAuth2ClientContext -> m ()
deleteOAuth2ClientContext subsite = N.delete $ ocxNonceGenerator subsite
