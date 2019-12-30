{-# LANGUAGE NoImplicitPrelude #-}

module Handler.SpotifyCallback(getSpotifyCallbackLoginR) where

import Foundation                   (Handler, Route(..), appSpotifyClientSubsite)
import OAuth2Client                 (handleCallback)
import Yesod.Auth                   (loginDest)
import Yesod.Core                   (getYesod, redirectUltDest)
import Import

import qualified Yesod.Auth.Message as AuthMsg


getSpotifyCallbackLoginR :: Handler ()
getSpotifyCallbackLoginR = do
    y <- getYesod
    handleCallback (appSpotifyClientSubsite y) SpotifyCallbackLoginR
    setMessageI AuthMsg.NowLoggedIn
    redirectUltDest $ loginDest y
