{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

module PlaylistTools
  ( PlaylistMonad(..)
  , ShuffleMonad(..)
  , createMergedPlaylist
  ) where

import Conduit

import Control.Monad  ((>=>))
import Data.Sequences (IsSequence)
import Data.Vector    (Vector)

import qualified Data.Sequences as S
import qualified VectorBuilder.Builder as VB
import qualified VectorBuilder.Vector as VB

import qualified RmeocUtils as Utils


class Monad m => PlaylistMonad m where
  type PlaylistId m
  type PlaylistName m
  type TrackId m
  createPlaylist :: PlaylistName m -> m (PlaylistId m)
  playlistTracksSource :: PlaylistId m -> ConduitT () (Vector (TrackId m)) m ()
  playlistTracksSink :: PlaylistId m -> ConduitT (Vector (TrackId m)) Void m ()

class Monad m => ShuffleMonad m where
  shuffle :: (IsSequence seq) => seq -> m seq


getPlaylistTracks :: (PlaylistMonad m) => PlaylistId m -> m (Vector (TrackId m))
getPlaylistTracks playlistId = do
    let pipeline = playlistTracksSource playlistId .| foldMapC VB.vector
    builder <- runConduit pipeline
    return $ VB.build builder

mergeSequencesShuffled :: (ShuffleMonad m, Functor f, Foldable f, IsSequence seq) => f seq -> m seq
mergeSequencesShuffled =
  let
    choose n xs = S.take n <$> shuffle xs
  in
    Utils.mergeSequencesBalanced choose >=> shuffle

createMergedPlaylist :: (PlaylistMonad m, ShuffleMonad m, Traversable f) => PlaylistName m -> f (PlaylistId m) -> m (PlaylistId m)
createMergedPlaylist playlistName sourcePlaylistIds = do

  playlistId <- createPlaylist playlistName

  sourceTracklists <- traverse getPlaylistTracks sourcePlaylistIds
  mergedTracklists <- mergeSequencesShuffled sourceTracklists

  let source = yield mergedTracklists
  let sink = playlistTracksSink playlistId
  let pipeline = source .| sink
  runConduit pipeline

  return playlistId
