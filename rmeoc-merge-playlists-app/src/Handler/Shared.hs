{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}

module Handler.Shared
    ( PlaylistPageParams(..)
    , SelectionParams(..)
    , playlistPageRequestParamsSpec
    , playlistPageRequestParams
    , selectionRequestParams
    , selectionRequestParamsSpec
    ) where

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

pageRefRequestParams :: PageRef -> [(Text,Text)]
pageRefRequestParams PageRef { pageRefDirection, pageRefOffset, pageRefLimit } =
    [   (requestParamNameDirection, toPathPiece $ Direction pageRefDirection)
    ,   (requestParamNameOffset, toPathPiece pageRefOffset)
    ,   (requestParamNameLimit, toPathPiece pageRefLimit)
    ]

data PlaylistPageParams = PlaylistPageParams { playlistPageParamsPageRef :: PageRef, playlistPageParamsOnlySelected :: Bool }

playlistPageRequestParamsSpec :: RequestParamsSpec PlaylistPageParams
playlistPageRequestParamsSpec = PlaylistPageParams <$> pageRefRequestParamsSpec <*> onlySelected
    where
        onlySelected :: RequestParamsSpec Bool
        onlySelected = requestParamSpec requestParamNameOnlySelected (Just False)

playlistPageRequestParams :: PlaylistPageParams -> [(Text,Text)]
playlistPageRequestParams PlaylistPageParams { playlistPageParamsPageRef, playlistPageParamsOnlySelected }
    = (requestParamNameOnlySelected, toPathPiece playlistPageParamsOnlySelected) : pageRefRequestParams playlistPageParamsPageRef

data SelectionParams = SelectionParams { selectionParamsPlaylistId :: PlaylistId, selectionParamsReturnToPage :: PlaylistPageParams }

selectionRequestParamsSpec :: RequestParamsSpec SelectionParams
selectionRequestParamsSpec = SelectionParams <$> playlistId <*> playlistPageRequestParamsSpec
    where
        playlistId :: RequestParamsSpec PlaylistId
        playlistId = requestParamSpec requestParamNamePlaylistId Nothing

selectionRequestParams :: SelectionParams -> [(Text,Text)]
selectionRequestParams SelectionParams { selectionParamsPlaylistId, selectionParamsReturnToPage } =
    (requestParamNamePlaylistId, toPathPiece selectionParamsPlaylistId) : playlistPageRequestParams selectionParamsReturnToPage
