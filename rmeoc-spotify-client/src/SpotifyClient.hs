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
    , Direction(..)
    , PageRef(..)
    , PlaylistId(..)
    , clonePlaylist
    , createPlaylist
    , getPlaylistPage
    , getPlaylistTracks
    , playlistTracksSink
    , playlistTracksSource
    ) where

import Conduit
import Control.Lens                 ((&), (.~), (?~), (^.))
import Control.Monad.IO.Class       (MonadIO, liftIO)
import Control.Monad.Reader         (MonadReader, ask)
import Data.Aeson                   (FromJSON, (.:), parseJSON)
import Data.ByteString.Builder      (intDec)
import Data.ByteString.Lazy         (ByteString)
import Data.Conduit.Combinators     (mapM_E, vectorBuilder)
import Data.Conduit.List            (unfoldM)
import Data.Maybe                   (catMaybes)
import Data.Text                    (Text)
import Data.Text.Encoding           (encodeUtf8)
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
import qualified Data.Vector as V
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as WS
import qualified Network.Wreq.Types as WT
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

supplyWreqOptionsAndSession :: (MonadReader S.SpotifyClientContext m) => (W.Options -> WS.Session -> m a) -> m a
supplyWreqOptionsAndSession f = do
    S.SpotifyClientContext { S.sccAccessToken, S.sccWreqSession } <- ask
    let wreqOptions = W.defaults & W.auth ?~ W.oauth2Bearer (encodeUtf8 sccAccessToken)
    f wreqOptions sccWreqSession

get' :: (MonadIO m, MonadReader S.SpotifyClientContext m) => String -> m (W.Response ByteString) 
get' url = supplyWreqOptionsAndSession $ \options session -> liftIO $ WS.getWith options session url

post' :: (MonadIO m, MonadReader S.SpotifyClientContext m, WT.Postable a) => String -> a -> m (W.Response ByteString) 
post' url payload = supplyWreqOptionsAndSession $ \options session -> liftIO $ WS.postWith options session url payload

asJSON' :: (MonadIO m, FromJSON a) => W.Response ByteString -> m (W.Response a) 
asJSON' = liftIO . W.asJSON

getPlaylistTracks :: (MonadUnliftIO m, MonadReader S.SpotifyClientContext m) => PlaylistId -> m (Vector Text)
getPlaylistTracks playlistId = do
    let pipeline = playlistTracksSource playlistId pagingParams .| foldMapC VB.vector
    builder <- runConduit pipeline
    return $ VB.build builder
  where
    pagingParams = 
        S.SpotifyPagingParams
            { S.sppLimit = 100
            }


data Direction = Reverse | Forward deriving Show

data PageRef = PageRef { pageRefDirection :: Direction, pageRefOffset :: Int, pageRefLimit :: Int } deriving Show

reverseDirection :: Direction -> Direction
reverseDirection Reverse = Forward
reverseDirection Forward = Reverse

getPlaylistPage :: forall m. (MonadIO m, MonadReader S.SpotifyClientContext m) => (S.PlaylistSimplified -> Bool) -> PageRef -> m (Maybe PageRef, Vector S.PlaylistSimplified, Maybe PageRef)
getPlaylistPage filterPredicate pageRef = runConduit $ do
        hasBefore <- playlistsBehind .| not <$> nullCE
        (playlists, hasAfter) <- playlistsAhead .| ((,) <$> getPageItems <*> (not <$> nullCE))
        let
            (hasPrev, hasNext) = case pageRefDirection pageRef of
                Forward -> (hasBefore, hasAfter)
                Reverse -> (hasAfter, hasBefore)
            playlists' = fmap snd playlists
            playlists'' = case pageRefDirection pageRef of
                Forward -> playlists'
                Reverse -> V.reverse playlists'
        pure (if hasPrev then Just (prev playlists) else Nothing, playlists'', if hasNext then Just (next playlists) else Nothing)
    where
        mlast :: Vector a -> Maybe a
        mlast v = if V.null v then Nothing else Just (V.last v)

        prev :: Vector (Int, S.PlaylistSimplified) -> PageRef
        prev items =
            let
                pageRefOffset' = case pageRefDirection pageRef of
                    Forward -> pageRefOffset pageRef
                    Reverse -> maybe (pageRefOffset pageRef) fst (mlast items)
            in
                PageRef { pageRefDirection = Reverse, pageRefOffset = pageRefOffset', pageRefLimit = pageRefLimit pageRef }

        next :: Vector (Int, S.PlaylistSimplified) -> PageRef
        next items =
            let
                pageRefOffset' = case pageRefDirection pageRef of
                    Forward -> maybe (pageRefOffset pageRef) ((+ 1) . fst) (mlast items)
                    Reverse -> pageRefOffset pageRef
            in
                PageRef { pageRefDirection = Forward, pageRefOffset = pageRefOffset', pageRefLimit = pageRefLimit pageRef }

        getPageItems :: ConduitT (Vector (Int, S.PlaylistSimplified)) o m (Vector (Int, S.PlaylistSimplified))
        getPageItems = mconcat <$> (takeCE (pageRefLimit pageRef) .| sinkList)

        playlistsBehind :: ConduitT () (Vector (Int, S.PlaylistSimplified)) m ()
        playlistsBehind = playlistsSource filterPredicate (reverseDirection $ pageRefDirection pageRef) (pageRefOffset pageRef) pagingParams

        playlistsAhead :: ConduitT () (Vector (Int, S.PlaylistSimplified)) m ()
        playlistsAhead = playlistsSource filterPredicate (pageRefDirection pageRef) (pageRefOffset pageRef) pagingParams

        pagingParams = 
            S.SpotifyPagingParams
                { S.sppLimit = 5
                }

playlistsSource :: (MonadIO m, MonadReader S.SpotifyClientContext m) => (S.PlaylistSimplified -> Bool) -> Direction -> Int -> S.SpotifyPagingParams -> ConduitT () (Vector (Int, S.PlaylistSimplified)) m ()
playlistsSource filterPredicate = pagedItemsSource getItemsUri parseItem
    where
        getItemsUri :: Int -> Int -> URIRef Absolute
        getItemsUri offset limit =
            [uri|https://api.spotify.com/v1/me/playlists|]
                & queryL .queryPairsL .~ 
                    catMaybes
                        [ (,) "offset" . BL.toByteString . intDec <$> Just offset
                        , (,) "limit" . BL.toByteString . intDec <$> Just limit
                        ]
        parseItem :: Int -> J.Object -> J.Parser (Maybe (Int, S.PlaylistSimplified))
        parseItem offset item = do
            playlist <- parseJSON (J.Object item)
            pure $ if filterPredicate playlist then Just (offset, playlist) else Nothing

playlistTracksSource :: (MonadIO m, MonadReader S.SpotifyClientContext m) => PlaylistId -> S.SpotifyPagingParams -> ConduitT () (Vector Text) m ()
playlistTracksSource playlistId = pagedItemsSource getItemsUri parseItem Forward 0
    where
        getItemsUri :: Int -> Int -> URIRef Absolute
        getItemsUri offset limit =
            [uri|https://api.spotify.com/|]
                & pathL .~ (encodeUtf8 $ "/v1/playlists/" <> S.unPlaylistId playlistId <> "/tracks")
                & queryL .queryPairsL .~ 
                    catMaybes
                        [ (,) "fields" <$> Just "items(track(uri)),total"
                        , (,) "offset" . BL.toByteString . intDec <$> Just offset
                        , (,) "limit" . BL.toByteString . intDec <$> Just limit
                        ]

        parseItem :: Int -> J.Object -> J.Parser (Maybe Text)
        parseItem _ item = do
            track <- item .: "track"
            Just <$> track .: "uri"

pagedItemsSource :: forall m a. (MonadIO m, MonadReader S.SpotifyClientContext m) =>
    (Int -> Int -> URIRef Absolute)
    -> (Int -> J.Object -> J.Parser (Maybe a))
    -> Direction
    -> Int
    -> S.SpotifyPagingParams
    -> ConduitT () (Vector a) m ()
pagedItemsSource getItemsUri parseItem direction startPos pagingParams = 
        unfoldM (maybe (pure Nothing) getNextChunk) (Just startPos)
    where
        limit :: Int
        limit = S.sppLimit pagingParams

        requestItems :: Int -> Int -> m (Vector a, Int)
        requestItems offset numItems = do
            resp <- doRequest offset numItems 
            parseResponse offset resp

        doRequest :: Int -> Int -> m J.Object
        doRequest offset numItems = do
            let url = getItemsUri offset numItems
            resp <- asJSON' =<< get' (BS8.unpack $ serializeURIRef' url)
            return $ resp ^. responseBody

        parseItems :: Int -> Vector J.Object -> J.Parser (Vector a)
        parseItems offset items =
            let
                x = V.mapMaybe id <$> V.imapM (parseItem . (+ offset)) (items :: Vector J.Object)
            in
                case direction of
                    Forward -> x
                    Reverse -> V.reverse <$> x

        parseResponse :: Int -> J.Object -> m (Vector a, Int)
        parseResponse offset resp = do
            either throwString return $ do
                flip J.parseEither resp $ \obj -> do
                    items <- obj .: "items"
                    uris <- parseItems offset items
                    total <- obj .: "total"
                    return (uris, total)

        getNextChunk :: Int -> m (Maybe (Vector a, Maybe Int))
        getNextChunk offset = do
            case direction of
                Forward -> getNextChunkForward offset
                Reverse -> getNextChunkReverse offset

        getNextChunkForward :: Int -> m (Maybe (Vector a, Maybe Int))
        getNextChunkForward offset = do
            let offset' = (max 0 offset)
            (items, total) <- requestItems offset' limit
            pure $
                if offset' >= total
                    then Nothing
                    else
                        Just
                            (   items
                            ,   let
                                    offset'' = offset' + limit
                                in
                                    if offset'' >= total
                                        then Nothing
                                        else Just offset''
                            )

        getNextChunkReverse :: Int -> m (Maybe (Vector a, Maybe Int))
        getNextChunkReverse offset = 
            let
                offset' = max 0 (offset - limit)
                numItems = (offset - offset')
            in
                if numItems <= 0
                    then
                        pure Nothing
                    else do
                        (items, total) <- requestItems offset' numItems
                        pure $ Just (items, Just $ min total offset')

playlistTracksSink :: (MonadIO m, MonadReader S.SpotifyClientContext m, PrimMonad m) => PlaylistId -> ConduitT (Vector Text) Void m ()
playlistTracksSink playlistId =
        rechunk .| sendPostRequestsSink
    where
        rechunk :: PrimMonad m => ConduitT (Vector Text) (Vector Text) m ()
        rechunk = vectorBuilder 100 mapM_E

        sendPostRequestsSink :: (MonadIO m, MonadReader S.SpotifyClientContext m) => ConduitT (Vector Text) Void m ()
        sendPostRequestsSink =
            awaitForever $ \tracks -> do
                post'
                    (BS8.unpack $ serializeURIRef' $ [uri|https://api.spotify.com/|]
                        & pathL .~ (encodeUtf8 $ "/v1/playlists/" <> S.unPlaylistId playlistId <> "/tracks"))
                    (J.object
                        [ ( "uris", J.Array $ fmap J.String tracks)
                        ])

createPlaylist :: (MonadIO m, MonadReader S.SpotifyClientContext m) => Text -> m CreatePlaylistResult
createPlaylist name = do

    resp <-
        asJSON' =<< post' "https://api.spotify.com/v1/me/playlists"
            (J.object
                [ ( "name", J.String name )
                , ( "public", J.Bool False )
                ])
                
    return $ CreatePlaylistResult <$> S.plafulId <*> S.plafulName $ resp ^. responseBody

clonePlaylist :: (MonadIO m, MonadReader S.SpotifyClientContext m, PrimMonad m) => ClonePlaylistParameters -> m CreatePlaylistResult
clonePlaylist params = do

    cpr <- createPlaylist (cppName params)

    let source = playlistTracksSource (cppSourcePlayListId params) pagingParams
    let sink = playlistTracksSink (cprId cpr)
    let pipeline = source .| sink

    runConduit pipeline

    return cpr
  where
    pagingParams = 
        S.SpotifyPagingParams
            { S.sppLimit = 100
            }
