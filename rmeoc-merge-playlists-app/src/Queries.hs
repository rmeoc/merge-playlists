{-# LANGUAGE NoImplicitPrelude         #-}

module Queries
    ( selectedPlaylistsQuery
    ) where

import SpotifyClient.Types

import Import


selectedPlaylistsQuery :: MonadResource m => UserId -> SqlPersistT m (Set PlaylistId)
selectedPlaylistsQuery userId = 
    runConduit $ 
        selectSource [Filter PlaylistSelectionUser (Left userId) Eq] []
        .| mapC (PlaylistId . playlistSelectionPlaylist . entityVal)
        .| foldMapC singleton
