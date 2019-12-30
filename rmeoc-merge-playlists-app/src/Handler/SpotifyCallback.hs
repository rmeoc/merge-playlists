{-# LANGUAGE NoImplicitPrelude #-}

module Handler.SpotifyCallback(getSpotifyCallbackR) where

import Foundation                   (Handler, Route(..), appSpotifyClientSubsite)
import OAuth2Client                 (handleCallback)
import Yesod.Core                   (getYesod)


getSpotifyCallbackR :: Handler ()
getSpotifyCallbackR = do
    y <- getYesod
    handleCallback (appSpotifyClientSubsite y) SpotifyCallbackR HomeR
