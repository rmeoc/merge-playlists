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
import OAuth2Client
import SpotifyClient hiding (Direction)
import SpotifyClient.Types
import Yesod.Core

import Handler.Shared
import Import
import RequestParams


runSpotify :: (MonadHandler m, MonadUnliftIO m, HandlerSite m ~ App) => ReaderT SpotifyClientContext m b -> ReaderT Session m b
runSpotify mx = do
    y <- getYesod
    token <- getAccessToken (appSpotifyClientContext y)
    withReaderT (flip SpotifyClientContext token) mx

getPlaylistsR :: Handler Html
getPlaylistsR = do
        pageRef <- parseGetParams pageRefRequestParamsSpec
        wreqSession <- liftIO newSession
        selectedPlaylists <- getSelectedPlaylists
        (mprev, playlists, mnext) <- runReaderT (runSpotify $ getPlaylistPage pageRef) wreqSession

        defaultLayout $ 
            [whamlet|
                <h1>Playlists
                $maybe prev <- mprev
                    <p>
                        <a href=@?{(PlaylistsR, pageRefRequestParams prev)}>
                            Previous Page
                $maybe next <- mnext
                    <p>
                        <a href=@?{(PlaylistsR, pageRefRequestParams next)}>
                            Next Page
                <ul>
                    $forall playlist <- playlists
                        ^{playlistWidget playlist pageRef selectedPlaylists}
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
        
        playlistWidget :: PlaylistSimplified -> PageRef-> Set PlaylistId -> Widget
        playlistWidget playlist pageRef selectedPlaylists = do
                [whamlet|
                    $with owner <- plasimOwner playlist
                        <li>
                            #{plasimName playlist}
                            <ul>
                                $maybe image <- chooseImage playlist
                                    <li>
                                        <img src=#{imaUrl image}>
                                <li> owner: #{fromMaybe (usepubId owner) (usepubDisplayName owner)}
                            $if isSelected
                                ^{selectionButton SelectionRemoveR "Remove from Selection"}
                            $else
                                ^{selectionButton SelectionAddR "Add to Selection"}
                |]
            where
                isSelected :: Bool
                isSelected = plasimId playlist `member` selectedPlaylists

                selectionButton :: Route App -> Text -> WidgetFor App ()
                selectionButton route text = postButton route (selectionRequestParams $ SelectionParams (plasimId playlist) pageRef) text


getSelectedPlaylists :: Handler (Set PlaylistId)
getSelectedPlaylists = do
    userId <- requireAuthId
    runDB $
        runConduit $
            selectSource [Filter PlaylistSelectionUser (Left userId) Eq] []
            .| mapC (PlaylistId . playlistSelectionPlaylist . entityVal)
            .| foldMapC singleton
