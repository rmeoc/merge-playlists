{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Handler.Playlists(getPlaylistsR) where

import Control.Monad.Reader         (ReaderT(..), runReaderT, withReaderT)
import Data.List                    (sortBy)
import Data.Maybe                   (fromMaybe, listToMaybe)
import Data.Ord                     (Down(..), comparing)
import Data.Text                    (Text)
import Foundation                   (App, Handler, Route(..), Widget, appSpotifyClientContext)
import OAuth2Client                 (getAccessToken)
import UnliftIO                     (MonadUnliftIO)
import Yesod.Core                   (HandlerSite, Html, MonadHandler, defaultLayout, getRequest,
                                     getYesod, liftIO, reqGetParams, whamlet)

import qualified Network.Wreq.Session as WS
import RequestParams
import qualified SpotifyClient as S
import qualified SpotifyClient.Types as S


runSpotify :: (MonadHandler m, MonadUnliftIO m, HandlerSite m ~ App) => ReaderT S.SpotifyClientContext m b -> ReaderT WS.Session m b
runSpotify mx = do
    y <- getYesod
    token <- getAccessToken (appSpotifyClientContext y)
    withReaderT (flip S.SpotifyClientContext token) mx

getPlaylistsR :: Handler Html
getPlaylistsR = do
        pageRef <- runParserGet pageRefParser
        wreqSession <- liftIO WS.newSession
        (mprev, playlists, mnext) <- runReaderT (runSpotify $ S.getPlaylistPage pageRef) wreqSession

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
        playlistsPageRoute :: S.PageRef -> (Route App, [(Text, Text)])
        playlistsPageRoute pageRef = (PlaylistsR, pageRefToQueryString pageRef)

        chooseImage :: S.PlaylistSimplified -> Maybe S.Image
        chooseImage = listToMaybe . sortBy compareImages . S.plasimImages

        compareImages :: S.Image -> S.Image -> Ordering
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
                    .   S.imaWidth
                    )
            where
                preferredImageWidth :: Integer
                preferredImageWidth = 300
        
        playlistWidget :: S.PlaylistSimplified -> Widget
        playlistWidget playlist = do
            [whamlet|
                $with owner <- S.plasimOwner playlist
                    <li>
                        #{S.plasimName playlist}
                        <ul>
                            $maybe image <- chooseImage playlist
                                <li>
                                    <img src=#{S.imaUrl image}>
                            <li> owner: #{fromMaybe (S.usepubId owner) (S.usepubDisplayName owner)}
            |]

fieldDirection :: Text
fieldDirection = "direction"

fieldOffset :: Text
fieldOffset = "offset"

fieldLimit :: Text
fieldLimit = "limit"

pageRefParser :: RequestParams.Parser S.PageRef
pageRefParser = S.PageRef <$> (toSpotifyClientDirection <$> direction) <*> offset <*> limit
    where
        direction :: RequestParams.Parser RequestParams.Direction
        direction = RequestParams.field RequestParams.parseDirection fieldDirection (RequestParams.Direction S.Forward)

        offset :: RequestParams.Parser Int
        offset = RequestParams.field RequestParams.parseInt fieldOffset 0

        limit :: RequestParams.Parser Int
        limit = RequestParams.field RequestParams.parseInt fieldLimit 10

pageRefToQueryString :: S.PageRef -> [(Text,Text)]
pageRefToQueryString S.PageRef { S.pageRefDirection, S.pageRefOffset, S.pageRefLimit } =
    [   (fieldDirection, RequestParams.printDirection $ Direction pageRefDirection)
    ,   (fieldOffset, RequestParams.printInt pageRefOffset)
    ,   (fieldLimit, RequestParams.printInt pageRefLimit)
    ]
