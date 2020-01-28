{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Handler.Playlists(getPlaylistsR) where

import Control.Monad.Reader
import Data.List
import Data.Maybe
import Data.Ord
import Data.Text
import Network.Wreq.Session
import OAuth2Client
import SpotifyClient hiding (Direction)
import SpotifyClient.Types
import UnliftIO hiding (Handler)
import Yesod.Core

import Direction
import Foundation
import RequestParams


runSpotify :: (MonadHandler m, MonadUnliftIO m, HandlerSite m ~ App) => ReaderT SpotifyClientContext m b -> ReaderT Session m b
runSpotify mx = do
    y <- getYesod
    token <- getAccessToken (appSpotifyClientContext y)
    withReaderT (flip SpotifyClientContext token) mx

getPlaylistsR :: Handler Html
getPlaylistsR = do
        pageRef <- parseFormGet pageRefFormSpec
        wreqSession <- liftIO newSession
        (mprev, playlists, mnext) <- runReaderT (runSpotify $ getPlaylistPage pageRef) wreqSession

        defaultLayout $ 
            [whamlet|
                <h1>Playlists
                $maybe prev <- mprev
                    <p>
                        <a href=@?{playlistsPageRoute prev}>
                            Previous Page
                $maybe next <- mnext
                    <p>
                        <a href=@?{playlistsPageRoute next}>
                            Next Page
                <ul>
                    $forall playlist <- playlists
                        ^{playlistWidget playlist}
            |]
    where
        playlistsPageRoute :: PageRef -> (Route App, [(Text, Text)])
        playlistsPageRoute pageRef = (PlaylistsR, pageRefToQueryString pageRef)

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
        
        playlistWidget :: PlaylistSimplified -> Widget
        playlistWidget playlist = do
            [whamlet|
                $with owner <- plasimOwner playlist
                    <li>
                        #{plasimName playlist}
                        <ul>
                            $maybe image <- chooseImage playlist
                                <li>
                                    <img src=#{imaUrl image}>
                            <li> owner: #{fromMaybe (usepubId owner) (usepubDisplayName owner)}
            |]

fieldDirection :: Text
fieldDirection = "direction"

fieldOffset :: Text
fieldOffset = "offset"

fieldLimit :: Text
fieldLimit = "limit"

pageRefFormSpec :: FormSpec PageRef
pageRefFormSpec = PageRef <$> (toSpotifyClientDirection <$> direction) <*> offset <*> limit
    where
        direction :: FormSpec Direction
        direction = RequestParams.field fieldDirection (Just $ Direction Forward)

        offset :: FormSpec Int
        offset = RequestParams.field fieldOffset (Just 0)

        limit :: FormSpec Int
        limit = RequestParams.field fieldLimit (Just 10)

pageRefToQueryString :: PageRef -> [(Text,Text)]
pageRefToQueryString PageRef { pageRefDirection, pageRefOffset, pageRefLimit } =
    [   (fieldDirection, toPathPiece $ Direction pageRefDirection)
    ,   (fieldOffset, toPathPiece pageRefOffset)
    ,   (fieldLimit, toPathPiece pageRefLimit)
    ]
