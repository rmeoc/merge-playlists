{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}

module Handler.Merge
    ( getMergeR
    , postMergeR
    ) where

import Control.Concurrent.STM
import Data.Random
import Data.Set hiding (fromList)
import Data.StateRef
import Network.Wreq.Session
import PlaylistTools
import SpotifyClient
import SpotifyClient.Types
import System.Random.Mersenne.Pure64

import Import
import Queries
import SpotifyUtils


newtype MyMonad a = MyMonad
    { runMyMonad :: ReaderT SpotifyClientContext Handler a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader SpotifyClientContext)

instance PlaylistMonad MyMonad where
    type PlaylistId MyMonad = SpotifyClient.PlaylistId
    type PlaylistName MyMonad = Text
    type TrackId MyMonad = Text
    createPlaylist playlistName = do
        result <- SpotifyClient.createPlaylist playlistName
        return $ SpotifyClient.cprId result

    playlistTracksSource = SpotifyClient.playlistTracksSource SpotifyPagingParams { sppLimit = 100 }

    playlistTracksSink = SpotifyClient.playlistTracksSink

instance ShuffleMonad MyMonad where
    shuffle xs = do
        y <- MyMonad getYesod
        let randomSource :: Ref STM PureMT
            randomSource = Ref $ appRandomGeneratorState y
        xs' <- runRVar (shuffleN (olength xs) (otoList xs)) randomSource
        return $ fromList xs'

instance PrimMonad MyMonad where
    type PrimState MyMonad = RealWorld
    primitive = MyMonad . primitive

getMergeR :: Handler Html
getMergeR = do
    (widget, enctype) <- generateFormPost form
    showForm widget enctype

postMergeR :: Handler Html
postMergeR = do
    ((result, widget), enctype) <- runFormPost form
    case result of
        FormSuccess playlistName -> do
            userId <- requireAuthId
            selectedPlaylists <- runDB $ selectedPlaylistsQuery userId
            wreqSession <- liftIO newSession
            void $
                flip runReaderT wreqSession $
                    runSpotify $
                        runMyMonad $
                            PlaylistTools.createMergedPlaylist playlistName (elems selectedPlaylists)
            urlRender <- getUrlRenderParams
            setMessage $ 
                [hamlet|
                    Created merged playlist.
                |]
                urlRender
            redirect PlaylistsR
        _ -> showForm widget enctype

form :: Html -> MForm Handler (FormResult Text, Widget) 
form = renderDivs $ areq textField ("Name" { fsName = Just "name" }) Nothing

showForm :: Widget -> Enctype -> Handler Html
showForm formWidget enctype = do
    defaultLayout $ 
        [whamlet|
            <h1>Create Merged Playlist
            <form method=post action=@{MergeR} enctype=#{enctype}>
                ^{formWidget}
                <button>Save
        |]
