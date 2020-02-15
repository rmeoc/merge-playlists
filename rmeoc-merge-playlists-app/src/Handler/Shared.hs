{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TypeOperators             #-}

module Handler.Shared
    ( PlaylistPageParams(..)
    , SelectionParams(..)
    , playlistPageRequestParamsSpec
    , selectionRequestParamsSpec
    ) where

import Control.Invertible.Monoidal
import RequestParams
import SpotifyClient

import Direction
import Import


pageRefRequestParamsSpec :: RequestParamsSpec PageRef
pageRefRequestParamsSpec = liftI3 [biCase| (Direction.Direction x, y, z) <-> PageRef x y z |] direction offset limit
    where
        direction :: RequestParamsSpec Direction.Direction
        direction = requestParam "direction" (Just $ Direction Forward)

        offset :: RequestParamsSpec Int
        offset = requestParam "offset" (Just 0)

        limit :: RequestParamsSpec Int
        limit = requestParam "limit" (Just 10)

data PlaylistPageParams = PlaylistPageParams { playlistPageParamsPageRef :: PageRef, playlistPageParamsOnlySelected :: Bool }

playlistPageRequestParamsSpec :: RequestParamsSpec PlaylistPageParams
playlistPageRequestParamsSpec = liftI2 [biCase| (x, y) <-> PlaylistPageParams x y |] pageRefRequestParamsSpec onlySelected
    where
        onlySelected :: RequestParamsSpec Bool
        onlySelected = requestParam "only-selected" (Just False)

data SelectionParams = SelectionParams { selectionParamsPlaylistId :: PlaylistId, selectionParamsReturnToPage :: PlaylistPageParams }

selectionRequestParamsSpec :: RequestParamsSpec SelectionParams
selectionRequestParamsSpec = liftI2 [biCase| (x, y) <-> SelectionParams x y |] playlistId playlistPageRequestParamsSpec
    where
        playlistId :: RequestParamsSpec PlaylistId
        playlistId = requestParam "playlist-id" Nothing
