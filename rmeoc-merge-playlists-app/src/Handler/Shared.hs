{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}

module Handler.Shared
    ( SelectionParams(..)
    , pageRefRequestParams
    , pageRefRequestParamsSpec
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

data SelectionParams = SelectionParams { selectionParamsPlaylistId :: PlaylistId, selectionParamsReturnToPage :: PageRef }

selectionRequestParamsSpec :: RequestParamsSpec SelectionParams
selectionRequestParamsSpec = SelectionParams <$> playlistId <*> pageRefRequestParamsSpec
    where
        playlistId :: RequestParamsSpec PlaylistId
        playlistId = requestParamSpec requestParamNamePlaylistId Nothing

selectionRequestParams :: SelectionParams -> [(Text,Text)]
selectionRequestParams SelectionParams { selectionParamsPlaylistId, selectionParamsReturnToPage } =
    (requestParamNamePlaylistId, toPathPiece selectionParamsPlaylistId) : pageRefRequestParams selectionParamsReturnToPage
