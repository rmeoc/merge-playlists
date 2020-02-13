{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Handler.Playlists(getPlaylistsR) where

import Conduit
import Control.Monad.Reader
import Data.Maybe
import Data.Ord
import Data.Text hiding (singleton)
import Network.Wreq.Session
import SpotifyClient hiding (Direction)
import SpotifyClient.Types
import Yesod.Core

import Handler.Shared
import Import
import Queries
import RequestParams
import SpotifyUtils


getPlaylistsR :: Handler Html
getPlaylistsR = do
        playlistPageParams <- parseGetParams playlistPageRequestParamsSpec

        let onlySelected = playlistPageParamsOnlySelected playlistPageParams
        userId <- requireAuthId
        selectedPlaylists <- runDB $ selectedPlaylistsQuery userId
        let filterPredicate = case onlySelected of
                False -> const True
                True -> \playlist -> plasimId playlist `member` selectedPlaylists

        let pageRef = playlistPageParamsPageRef playlistPageParams
        wreqSession <- liftIO newSession
        (mprev, playlists, mnext) <- runReaderT (runSpotify $ getPlaylistPage filterPredicate pageRef) wreqSession

        defaultLayout $ 
            [whamlet|
                <h1>Playlists
                <p>
                    <a href=@?{(PlaylistsR, runRequestParamSerializer playlistPageRequestParams $ PlaylistPageParams pageRef (not onlySelected))}>
                        $if onlySelected
                            Display All
                        $else
                            Only Display Selected
                $maybe prev <- mprev
                    <p>
                        <a href=@?{(PlaylistsR, runRequestParamSerializer playlistPageRequestParams $ PlaylistPageParams prev onlySelected)}>
                            Previous Page
                $maybe next <- mnext
                    <p>
                        <a href=@?{(PlaylistsR, runRequestParamSerializer playlistPageRequestParams $ PlaylistPageParams next onlySelected)}>
                            Next Page
                <ul>
                    $forall playlist <- playlists
                        ^{playlistWidget playlist playlistPageParams selectedPlaylists}
            |]
    where
        chooseImage :: PlaylistSimplified -> Maybe Image
        chooseImage = listToMaybe . sortBy compareImages . plasimImages

        compareImages :: Image -> Image -> Ordering
        compareImages =
                comparing
                    (maybe
                        (Right ())
                        ( Left
                        . (\width ->
                            ( Down $ width >= preferredImageWidth
                            , abs $ width - preferredImageWidth
                            )
                        )
                        )
                    .   imaWidth
                    )
            where
                preferredImageWidth :: Integer
                preferredImageWidth = 300
        
        playlistWidget :: PlaylistSimplified -> PlaylistPageParams-> Set PlaylistId -> Widget
        playlistWidget playlist playlistPageParams selectedPlaylists = do
                [whamlet|
                    $with owner <- plasimOwner playlist
                        <li>
                            #{plasimName playlist}
                            <ul>
                                $maybe image <- chooseImage playlist
                                    <li>
                                        <img src=#{imaUrl image}>
                                <li>owner: #{fromMaybe (usepubId owner) (usepubDisplayName owner)}
                                <li>number of tracks: #{tracksTotal $ plasimTracks playlist}
                            $if isSelected
                                ^{selectionButton SelectionRemoveR "Remove from Selection"}
                            $else
                                ^{selectionButton SelectionAddR "Add to Selection"}
                |]
            where
                isSelected :: Bool
                isSelected = plasimId playlist `member` selectedPlaylists

                selectionButton :: Route App -> Text -> WidgetFor App ()
                selectionButton route text = postButton route (runRequestParamSerializer selectionRequestParams $ SelectionParams (plasimId playlist) playlistPageParams) text
