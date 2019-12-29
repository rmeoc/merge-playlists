{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Auth0
    ( Auth0Settings(..)
    , auth0Plugin
    ) where

import ClassyPrelude.Yesod

import Control.Lens                 ((.~), (&), (^.), (#), _Just, review)
import Control.Monad.Except         (ExceptT, runExceptT)
import Crypto.JWT                   (Alg(..), ClaimsSet, HasAlgorithms(..), HasAllowedSkew(..), HasIssuerPredicate(..),
                                     HasCheckIssuedAt(..), HasValidationPolicy(..), JWTError, JWTValidationSettings,
                                     StringOrURI, ValidationPolicy(..), algorithms, claimSub, decodeCompact,
                                     defaultJWTValidationSettings, fromOctets, stringOrUri, validationPolicy,
                                     verifyClaims)
import Data.Aeson                   ((.:?), (.!=), withObject)
import Data.Time.Clock              (NominalDiffTime)
import Network.OAuth.OAuth2         (idtoken)
import URI.ByteString               (Absolute, URIRef, authorityL,  authorityHostL, hostBSL, serializeURIRef')
import URI.ByteString.QQ            (uri)
import Yesod.Auth                   (AuthPlugin, Creds(..), YesodAuth)
import Yesod.Auth.OAuth2            (OAuth2(..), authOAuth2, idToken)

import qualified Yesod.Auth.OAuth2.Exception as YesodOAuth2Exception


data Auth0Settings = Auth0Settings
    { auth0ClientId     :: StringOrURI
    , auth0ClientSecret :: Text
    , auth0Domain       :: Text
    , auth0AllowedSkew  :: NominalDiffTime
    }

instance FromJSON Auth0Settings where
    parseJSON = withObject "Auth0Settings" $ \o -> do
        auth0ClientId             <- o .: "client-id"
        auth0ClientSecret         <- o .: "client-secret"
        auth0Domain               <- o .: "domain"
        auth0AllowedSkew          <- o .:? "allowed-skew" .!= 0
        return $ Auth0Settings {..}
    
pluginName :: Text
pluginName = "Auth0"

auth0Plugin :: YesodAuth m => Auth0Settings -> AuthPlugin m
auth0Plugin Auth0Settings {..}
    = authOAuth2
        pluginName
        OAuth2
            { oauthClientId = stringOrUri # auth0ClientId
            , oauthClientSecret = auth0ClientSecret
            , oauthOAuthorizeEndpoint = setDomain [uri|https://example.com/authorize?scope=openid|]
            , oauthAccessTokenEndpoint = setDomain [uri|https://example.com/oauth/token|]
            , oauthCallback = Nothing
            }
       (\_ token -> maybe (throwIO $ YesodOAuth2Exception.GenericError pluginName "Did not receive id token")
                          (getCredsFromIdToken . idtoken)
                          (idToken token))
  where
    setDomain :: URIRef Absolute -> URIRef Absolute
    setDomain = authorityL . _Just . authorityHostL . hostBSL .~ encodeUtf8 auth0Domain

    myJWTValidationSettings :: JWTValidationSettings
    myJWTValidationSettings =
        defaultJWTValidationSettings (== auth0ClientId)
            & algorithms .~ singleton HS256
            & validationPolicy .~ AllValidated
            & allowedSkew .~ auth0AllowedSkew
            & checkIssuedAt .~ True
            & issuerPredicate .~ (== requiredIssuer) . encodeUtf8 . review stringOrUri
      where
        requiredIssuer :: ByteString
        requiredIssuer = serializeURIRef' $ [uri|https://example.com/|] & authorityL . _Just . authorityHostL . hostBSL .~ encodeUtf8 auth0Domain

    verifyIdToken :: Text -> ExceptT JWTError IO ClaimsSet
    verifyIdToken x = do
        let k = fromOctets $ encodeUtf8 auth0ClientSecret
        jws <- decodeCompact $ fromStrict $ encodeUtf8 x
        verifyClaims myJWTValidationSettings k jws

    getCredsFromIdToken :: Text -> IO (Creds m)
    getCredsFromIdToken x = do
        verificationResult <- runExceptT $ verifyIdToken x
        claims <- either (throwIO . YesodOAuth2Exception.GenericError pluginName . ("Invalid id token: " <>) . show)
                         return
                         verificationResult
        subject <- maybe (throwIO $ YesodOAuth2Exception.GenericError pluginName "Id token has no subject claim")
                         (return . review stringOrUri)
                         (claims ^. claimSub)
        return $ Creds pluginName subject []
