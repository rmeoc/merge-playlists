module OAuth2Client
    ( OAuth2ClientConf
    , OAuth2ClientSubsite
    , SessionKey(..)
    , handleCallback
    , initOAuth2ClientSubsite
    , redirectToAuthorizationPage
    , withAccessToken
    ) where
    
import OAuth2Client.Foundation
import OAuth2Client.Handler
