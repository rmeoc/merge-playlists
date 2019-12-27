{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TypeFamilies              #-}

module Handler.Playlists(getPlaylistsR) where

import Control.Monad                (guard)
import Control.Monad.Reader         (ReaderT, runReaderT, withReaderT)
import Data.List                    (sortBy)
import Data.Maybe                   (fromMaybe, listToMaybe)
import Data.Ord                     (Down(..), comparing)
import Data.Text                    (Text)
import Foundation                   (App, Handler, Route(..), Widget, appSpotifyClientSubsite)
import OAuth2Client                 (withAccessToken)
import SpotifyClient                (getListOfPlaylists)
import UnliftIO                     (MonadUnliftIO)
import Yesod.Core                   (HandlerSite, Html, MonadHandler, defaultLayout, getYesod, liftIO, whamlet)
import Yesod.Form                   (intField, iopt, runInputGet)

import qualified Data.Text as T
import qualified Network.Wreq.Session as WS
import qualified SpotifyClient.Types as S


runSpotify :: (MonadHandler m, MonadUnliftIO m, HandlerSite m ~ App) => ReaderT S.SpotifyClientContext m b -> ReaderT WS.Session m b
runSpotify mx = do
    y <- getYesod
    withAccessToken (appSpotifyClientSubsite y) SpotifyClientR $ \token -> 
        withReaderT (flip S.SpotifyClientContext token) mx

getPlaylistsR :: Handler Html
getPlaylistsR = do

    moffset <- runInputGet $ iopt intField fieldNameOffset
    let offset = fromMaybe 0 moffset

    wreqSession <- liftIO WS.newSession
    playlists <- runReaderT (runSpotify $ getListOfPlaylists offset pageSize) wreqSession

    defaultLayout $ 
        [whamlet|
            <h1>Playlists
            $maybe offsetPrev <- previousPageOffset offset pageSize
                <p>
                    <a href=@?{playlistsPageRoute offsetPrev}>
                        Previous Page
            $maybe offsetNext <- nextPageOffset offset pageSize (S.pagTotal playlists)
                <p>
                    <a href=@?{playlistsPageRoute offsetNext}>
                        Next Page
            <ul>
                $forall playlist <- S.pagItems playlists
                    ^{playlistWidget playlist}
        |]
    where
        fieldNameOffset :: Text
        fieldNameOffset = "offset"

        pageSize :: Integer
        pageSize = 10

        playlistsPageRoute :: Integer -> (Route App, [(Text, Text)])
        playlistsPageRoute offset = (PlaylistsR, [(fieldNameOffset, T.pack $ show offset)])

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

previousPageOffset :: Integer -> Integer -> Maybe Integer
previousPageOffset currentOffset pageSize = do
    guard $ currentOffset > 0
    return $ max 0 (currentOffset - pageSize)
                
nextPageOffset :: Integer -> Integer -> Integer -> Maybe Integer
nextPageOffset currentOffset pageSize total = do
    let newOffset = currentOffset + pageSize
    guard $ newOffset < total
    return newOffset
