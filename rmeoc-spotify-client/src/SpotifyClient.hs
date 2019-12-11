{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ViewPatterns              #-}

module SpotifyClient
    ( ClonePlaylistParameters(..)
    , CreatePlaylistResult(..)
    , PlaylistId
    , clonePlaylist
    , createPlaylist
    , getListOfPlaylists
    , getPlaylistTracks
    , playlistTracksSink
    , playlistTracksSource
    , unPlaylistId
    ) where

import Conduit
import Control.Applicative          (liftA2)
import Control.Lens                 ((&), (.~), (?~), (^.))
import Control.Monad                (when)
import Control.Monad.IO.Class       (MonadIO, liftIO)
import Control.Monad.Reader         (MonadReader, ask)
import Data.Aeson                   ((.:))
import Data.ByteString.Builder      (intDec)
import Data.Conduit.Combinators     (mapM_E, vectorBuilder)
import Data.List.NonEmpty           (NonEmpty(..))
import Data.Monoid                  (Sum(..))
import Data.Text                    (Text)
import Data.Text.Encoding           (encodeUtf8)
import Data.Traversable             (for)
import Data.Vector                  (Vector)
import Network.Wreq                 (responseBody)
import SpotifyClient.Types          (PlaylistId, unPlaylistId)
import UnliftIO                     (MonadUnliftIO, throwString)
import URI.ByteString               (Absolute, URIRef, pathL, queryL, queryPairsL, serializeURIRef')
import URI.ByteString.QQ            (uri)

import qualified Blaze.ByteString.Builder as BL
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as WS
import qualified SpotifyClient.Types as S
import qualified VectorBuilder.Builder as VB
import qualified VectorBuilder.Vector as VB

data ClonePlaylistParameters = ClonePlaylistParameters
    { cppName :: Text
    , cppSourcePlayListId :: PlaylistId
    }

data CreatePlaylistResult = CreatePlaylistResult
    { cprId :: PlaylistId
    , cprName :: Text
    }

getListOfPlaylists :: (MonadIO m, MonadReader S.SpotifyClientContext m) => Integer -> Integer -> m (S.Paging S.PlaylistSimplified)
getListOfPlaylists offset limit = do

    S.SpotifyClientContext { S.sccAccessToken, S.sccWreqSession } <- ask

    let wreqOptions = W.defaults & W.auth ?~ W.oauth2Bearer (encodeUtf8 sccAccessToken)

    playlistsResp <-
        liftIO $
            W.asJSON =<< (WS.getWith wreqOptions sccWreqSession $ 
                BS8.unpack $ serializeURIRef' $
                    [uri|https://api.spotify.com/v1/me/playlists|]
                        & queryL .queryPairsL .~ 
                            [ ("offset", BS8.pack $ show offset)
                            , ("limit", BS8.pack $ show limit)
                            ])

    return $ playlistsResp ^. responseBody    

getPlaylistTracks :: (MonadUnliftIO m, MonadReader S.SpotifyClientContext m) => PlaylistId -> m (Vector Text)
getPlaylistTracks playlistId = do
    let pipeline = playlistTracksSource playlistId .| foldMapC VB.vector
    builder <- runConduit pipeline
    return $ VB.build builder

playlistTracksSource :: (MonadIO m, MonadReader S.SpotifyClientContext m) => PlaylistId -> ConduitT () (Vector Text) m ()
playlistTracksSource playlistId = do

        let itemTrackUriField = "items(track(uri))"

        response1 <- requestTracks (itemTrackUriField :| ["total"]) 0
        tracks1 <- getTracksFromResponse response1
        yield tracks1
        
        totalNumberOfTracks <- either throwString return $
            flip J.parseEither response1 $ \obj -> obj .: "total"

        Sum numberOfTracksReceived <- foldr
            (\offset -> 
                liftA2 mappend $
                    do
                        resp <- requestTracks (itemTrackUriField :| []) offset
                        tracks <- getTracksFromResponse resp
                        yield tracks
                        return $ Sum $ V.length tracks)
            (pure $ Sum $ V.length tracks1)
            [maxNumberOfTrackPerRequest, 2*maxNumberOfTrackPerRequest ..totalNumberOfTracks-1]

        when (numberOfTracksReceived /= totalNumberOfTracks) $
            throwString $ "Number of tracks actually received (" <> show numberOfTracksReceived
                <> ") does not match number of tracks that was initially reported ("
                <> show totalNumberOfTracks <> ")."

        return ()

    where
        maxNumberOfTrackPerRequest :: Int
        maxNumberOfTrackPerRequest = 100

        getTracksUri :: NonEmpty BL.Builder -> Int -> URIRef Absolute
        getTracksUri fields offset =
            [uri|https://api.spotify.com/|]
                & pathL .~ (encodeUtf8 $ "/v1/playlists/" <> S.unPlaylistId playlistId <> "/tracks")
                & queryL .queryPairsL .~ 
                    [ ( "fields"
                        , BL.toByteString $ (NE.head fields) <> mconcat ["," <>  x  | x <- NE.tail fields]
                        )
                    , ( "offset"
                        , BL.toByteString $ intDec offset
                        )
                    , ( "limit"
                        , BL.toByteString $ intDec maxNumberOfTrackPerRequest
                        )
                    ]

        requestTracks :: (MonadIO m, MonadReader S.SpotifyClientContext m) => NonEmpty BL.Builder -> Int -> m J.Object
        requestTracks fields offset = do
            S.SpotifyClientContext { S.sccAccessToken, S.sccWreqSession } <- ask
            let url = getTracksUri fields offset
            let wreqOptions = W.defaults & W.auth ?~ W.oauth2Bearer (encodeUtf8 sccAccessToken)
            resp <- liftIO (W.asJSON =<< (WS.getWith wreqOptions sccWreqSession $ BS8.unpack $ serializeURIRef' url))
            return $ resp ^. responseBody
        
        getTracksFromResponse :: (MonadIO m) => J.Object -> m (Vector Text)
        getTracksFromResponse resp = 
            either throwString return $ do
                flip J.parseEither resp $ \obj -> do
                    items <- obj .: "items"
                    uris <- for (items :: Vector J.Object) $ \item -> do
                        track <- item .: "track"
                        track .: "uri" 
                    return uris

playlistTracksSink :: (MonadIO m, MonadReader S.SpotifyClientContext m, PrimMonad m) => PlaylistId -> ConduitT (Vector Text) Void m ()
playlistTracksSink playlistId =
        rechunk .| sendPostRequestsSink
    where
        rechunk :: PrimMonad m => ConduitT (Vector Text) (Vector Text) m ()
        rechunk = vectorBuilder 100 mapM_E

        sendPostRequestsSink :: (MonadIO m, MonadReader S.SpotifyClientContext m) => ConduitT (Vector Text) Void m ()
        sendPostRequestsSink =
            awaitForever $ \tracks -> do
                S.SpotifyClientContext { S.sccAccessToken, S.sccWreqSession } <- ask
                let wreqOptions = W.defaults & W.auth ?~ W.oauth2Bearer (encodeUtf8 sccAccessToken)
                liftIO $
                    WS.postWith wreqOptions sccWreqSession
                        (BS8.unpack $ serializeURIRef' $ [uri|https://api.spotify.com/|]
                            & pathL .~ (encodeUtf8 $ "/v1/playlists/" <> S.unPlaylistId playlistId <> "/tracks"))
                        (J.object
                            [ ( "uris", J.Array $ fmap J.String tracks)
                            ])

createPlaylist :: (MonadIO m, MonadReader S.SpotifyClientContext m) => Text -> m CreatePlaylistResult
createPlaylist name = do

    S.SpotifyClientContext { S.sccAccessToken, S.sccWreqSession } <- ask

    let wreqOptions = W.defaults & W.auth ?~ W.oauth2Bearer (encodeUtf8 sccAccessToken)

    resp <- liftIO $
        W.asJSON =<< (WS.postWith wreqOptions sccWreqSession "https://api.spotify.com/v1/me/playlists"
            $ J.object
                [ ( "name", J.String name )
                , ( "public", J.Bool False )
                ])
                
    return $ CreatePlaylistResult <$> S.plafulId <*> S.plafulName $ resp ^. responseBody

clonePlaylist :: (MonadIO m, MonadReader S.SpotifyClientContext m, PrimMonad m) => ClonePlaylistParameters -> m CreatePlaylistResult
clonePlaylist params = do

    cpr <- createPlaylist (cppName params)

    let source = playlistTracksSource (cppSourcePlayListId params)
    let sink = playlistTracksSink (cprId cpr)
    let pipeline = source .| sink

    runConduit pipeline

    return cpr
