{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}

module Handler.Shared
    ( PlaylistPageParams(..)
    , SelectionParams(..)
    , playlistPageRequestParamsSpec
    , playlistPageRequestParams
    , selectionRequestParams
    , selectionRequestParamsSpec
    ) where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Text
import RequestParams
import SpotifyClient

import Direction
import Import


requestParamNameDirection :: Text
requestParamNameDirection = "direction"

requestParamNameOffset :: Text
requestParamNameOffset = "offset"

requestParamNameLimit :: Text
requestParamNameLimit = "limit"

requestParamNamePlaylistId :: Text
requestParamNamePlaylistId = "playlist-id"

requestParamNameOnlySelected :: Text
requestParamNameOnlySelected = "only-selected"

pageRefRequestParamsSpec :: RequestParamsSpec PageRef
pageRefRequestParamsSpec = PageRef <$> (toSpotifyClientDirection <$> direction) <*> offset <*> limit
    where
        direction :: RequestParamsSpec Direction.Direction
        direction = requestParamSpec requestParamNameDirection (Just $ Direction Forward)

        offset :: RequestParamsSpec Int
        offset = requestParamSpec requestParamNameOffset (Just 0)

        limit :: RequestParamsSpec Int
        limit = requestParamSpec requestParamNameLimit (Just 10)

pageRefRequestParams :: RequestParamSerializer PageRef
pageRefRequestParams =
    divide
        (\PageRef { pageRefDirection, pageRefOffset, pageRefLimit } -> (pageRefDirection, (pageRefOffset, pageRefLimit)))
        (contramap Direction $ requestParam requestParamNameDirection)
        (divided
            (requestParam requestParamNameOffset)
            (requestParam requestParamNameLimit))

data PlaylistPageParams = PlaylistPageParams { playlistPageParamsPageRef :: PageRef, playlistPageParamsOnlySelected :: Bool }

playlistPageRequestParamsSpec :: RequestParamsSpec PlaylistPageParams
playlistPageRequestParamsSpec = PlaylistPageParams <$> pageRefRequestParamsSpec <*> onlySelected
    where
        onlySelected :: RequestParamsSpec Bool
        onlySelected = requestParamSpec requestParamNameOnlySelected (Just False)

playlistPageRequestParams :: RequestParamSerializer PlaylistPageParams
playlistPageRequestParams =
    divide
        (\PlaylistPageParams { playlistPageParamsPageRef, playlistPageParamsOnlySelected } -> (playlistPageParamsPageRef,playlistPageParamsOnlySelected))
        pageRefRequestParams
        (requestParam requestParamNameOnlySelected)

data SelectionParams = SelectionParams { selectionParamsPlaylistId :: PlaylistId, selectionParamsReturnToPage :: PlaylistPageParams }

selectionRequestParamsSpec :: RequestParamsSpec SelectionParams
selectionRequestParamsSpec = SelectionParams <$> playlistId <*> playlistPageRequestParamsSpec
    where
        playlistId :: RequestParamsSpec PlaylistId
        playlistId = requestParamSpec requestParamNamePlaylistId Nothing

selectionRequestParams :: RequestParamSerializer SelectionParams
selectionRequestParams =
    divide
        (\SelectionParams { selectionParamsPlaylistId, selectionParamsReturnToPage } -> (selectionParamsPlaylistId, selectionParamsReturnToPage))
        (requestParam requestParamNamePlaylistId)
        playlistPageRequestParams 
