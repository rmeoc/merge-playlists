{-# LANGUAGE TypeFamilies              #-}

module SpotifyUtils(runSpotify) where

import Control.Monad.Reader
import Network.Wreq.Session
import OAuth2Client
import SpotifyClient.Types

import Import


runSpotify :: (MonadHandler m, MonadUnliftIO m, HandlerSite m ~ App) => ReaderT SpotifyClientContext m b -> ReaderT Session m b
runSpotify mx = do
    y <- getYesod
    token <- getAccessToken (appSpotifyClientContext y)
    withReaderT (flip SpotifyClientContext token) mx
