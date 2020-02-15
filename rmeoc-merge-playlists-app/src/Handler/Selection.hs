{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

module Handler.Selection
    ( postSelectionAddR
    , postSelectionRemoveR
    ) where

import SpotifyClient
import Yesod.Core

import Handler.Shared
import Import
import RequestParams


postSelectionAddR :: Handler Html
postSelectionAddR = selectionHandler $ \userId playlistId -> insert_ $ PlaylistSelection userId (unPlaylistId playlistId)

postSelectionRemoveR :: Handler Html
postSelectionRemoveR = selectionHandler $ \userId playlistId -> delete $ PlaylistSelectionKey userId (unPlaylistId playlistId)

selectionHandler :: (UserId -> PlaylistId -> SqlPersistT Handler ()) -> Handler a
selectionHandler action = do
    SelectionParams { selectionParamsPlaylistId, selectionParamsReturnToPage } <- parsePostParams selectionRequestParamsSpec
    userId <- requireAuthId
    runDB $ action userId selectionParamsPlaylistId
    redirect (PlaylistsR, toRequestParams playlistPageRequestParamsSpec selectionParamsReturnToPage)
