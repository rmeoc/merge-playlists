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
        pageRef <- parseGetParams pageRefRequestParamsSpec
        wreqSession <- liftIO newSession
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
                        ^{playlistWidget playlist}
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

requestParamNameDirection :: Text
requestParamNameDirection = "direction"

requestParamNameOffset :: Text
requestParamNameOffset = "offset"

requestParamNameLimit :: Text
requestParamNameLimit = "limit"

pageRefRequestParamsSpec :: RequestParamsSpec PageRef
pageRefRequestParamsSpec = PageRef <$> (toSpotifyClientDirection <$> direction) <*> offset <*> limit
    where
        direction :: RequestParamsSpec Direction
        direction = requestParamSpec requestParamNameDirection (Just $ Direction Forward)

        offset :: RequestParamsSpec Int
        offset = requestParamSpec requestParamNameOffset (Just 0)

        limit :: RequestParamsSpec Int
        limit = requestParamSpec requestParamNameLimit (Just 10)

pageRefRequestParams :: PageRef -> [(Text,Text)]
pageRefRequestParams PageRef { pageRefDirection, pageRefOffset, pageRefLimit } =
    [   (requestParamNameDirection, toPathPiece $ Direction pageRefDirection)
    ,   (requestParamNameOffset, toPathPiece pageRefOffset)
    ,   (requestParamNameLimit, toPathPiece pageRefLimit)
    ]
