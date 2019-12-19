{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module OAuth2Client.Foundation
    ( OAuth2ClientConf(..)
    , OAuth2ClientSubsite(..)
    , Route(..)
    , SessionKey(..)
    , Url(..)
    , deleteOAuth2ClientSubsite
    , initOAuth2ClientSubsite
    , resourcesOAuth2ClientSubsite
    ) where

import Control.Monad.IO.Class   (MonadIO)
import Data.Aeson               (FromJSON, genericParseJSON, parseJSON, withText)
import Data.Aeson.Casing        (aesonPrefix, trainCase)
import Data.Either              (either)
import Data.Text                (Text)
import Data.Text.Encoding       (encodeUtf8)
import GHC.Generics             (Generic)
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
    , occTokenUrl :: String
    , occScopes :: [Text]
    } deriving (Generic)

instance FromJSON OAuth2ClientConf where
    parseJSON = genericParseJSON $ aesonPrefix trainCase

data SessionKey
    = SessionKeyAccessToken
    | SessionKeyRefreshToken
    | SessionKeyState
    | SessionKeyRetryingWithNewAccessToken

data OAuth2ClientSubsite = OAuth2ClientSubsite
    { ocsSessionKey :: SessionKey -> Text
    , ocsConfig :: OAuth2ClientConf
    , ocsNonceGenerator :: N.Generator
    }

mkYesodSubData "OAuth2ClientSubsite" $(parseRoutesFile "routes")

instance Yesod OAuth2ClientSubsite

initOAuth2ClientSubsite :: MonadIO m => (SessionKey -> Text) -> OAuth2ClientConf -> m OAuth2ClientSubsite
initOAuth2ClientSubsite translateSessionKey conf = do
    nonceGen <- N.new
    return $ OAuth2ClientSubsite translateSessionKey conf nonceGen

deleteOAuth2ClientSubsite :: MonadIO m => OAuth2ClientSubsite -> m ()
deleteOAuth2ClientSubsite subsite = N.delete $ ocsNonceGenerator subsite
