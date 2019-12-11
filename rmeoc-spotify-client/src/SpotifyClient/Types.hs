{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SpotifyClient.Types
    ( AlbumSimplified(..)
    , ArtistSimplified(..)
    , Followers(..)
    , Image(..)
    , Paging(..)
    , PlaylistFull(..)
    , PlaylistId(..)
    , PlaylistSimplified(..)
    , PlaylistTrack(..)
    , Restrictions(..)
    , SpotifyClientContext(..)
    , Track(..)
    , TrackLink(..)
    , Tracks(..)
    , UserPrivate(..)
    , UserPublic(..)
    ) where

import Data.Aeson               (FromJSON, genericParseJSON, parseJSON)
import Data.Aeson.Casing        (aesonPrefix, snakeCase)
import Data.HashMap.Strict      (HashMap)
import Data.Text                (Text)
import Data.Time.Clock          (UTCTime)
import GHC.Generics             (Generic)
import Web.PathPieces           (PathPiece)

import qualified Network.Wreq.Session as WS


data SpotifyClientContext = SpotifyClientContext
    { sccWreqSession :: WS.Session
    , sccAccessToken :: Text
    }

newtype PlaylistId = PlaylistId { unPlaylistId :: Text } deriving (Eq, FromJSON, PathPiece, Read, Show)

data AlbumSimplified = AlbumSimplified
    { albsimAlbumGroup :: Maybe Text
    , albsimAlbumType :: Text
    , albsimArtists :: [ArtistSimplified]
    , albsimAvailableMarkets :: [Text]
    , albsimExternalUrls :: HashMap Text Text
    , albsimHref :: Text
    , albsimId :: Text
    , albsimImages :: [Image]
    , albsimName :: Text
    , albsimReleaseDate :: Text
    , albsimReleaseDatePrecision :: Text
    , albsimRestrictions :: Restrictions
    , albsimType :: Text
    , albsimUri :: Text
    } deriving (Generic, Show)

instance FromJSON AlbumSimplified where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data ArtistSimplified = ArtistSimplified
    { artsimExternalUrls :: HashMap Text Text
    , artsimHref :: Text
    , artsimId :: Text
    , artsimName :: Text
    , artsimType :: Text
    , artsimUri :: Text
    } deriving (Generic, Show)

instance FromJSON ArtistSimplified where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Followers = Followers
    { folHref :: Maybe Text
    , folTotal :: Integer
    } deriving (Generic, Show)

instance FromJSON Followers where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase
    
data Image = Image
    { imaHeight :: Maybe Integer
    , imaUrl :: Text
    , imaWidth :: Maybe Integer
    } deriving (Generic, Show)

instance FromJSON Image where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Paging a = Paging
    { pagHref :: Text
    , pagItems :: [a]
    , pagLimit :: Integer
    , pagNext :: Maybe Text
    , pagOffset :: Integer
    , pagPrevious :: Maybe Text
    , pagTotal :: Integer
    } deriving (Generic, Show)

instance (FromJSON a) => FromJSON (Paging a) where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data PlaylistFull = PlaylistFull
    { plafulCollaborative :: Bool
    , plafulDescription :: Maybe Text
    , plafulExternalUrls :: HashMap Text Text
    , plafulFollowers :: Followers
    , plafulHref :: Text
    , plafulId :: PlaylistId
    , plafulImages :: [Image]
    , plafulName :: Text
    , plafulOwner :: UserPublic
    , plafulPublic :: Maybe Bool
    , plafulSnapshotId :: Text
    , plafulTracks :: Paging PlaylistTrack
    , plafulType :: Text
    , plafulUri :: Text
    } deriving (Generic, Show)

instance FromJSON PlaylistFull where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data PlaylistSimplified = PlaylistSimplified
    { plasimCollaborative :: Bool
    , plasimExternalUrls :: HashMap Text Text
    , plasimHref :: Text
    , plasimId :: PlaylistId
    , plasimImages :: [Image]
    , plasimName :: Text
    , plasimOwner :: UserPublic
    , plasimPublic :: Maybe Bool
    , plasimSnapshotId :: Text
    , plasimTracks :: Tracks
    , plasimType :: Text
    , plasimUri :: Text
    } deriving (Generic, Show)

instance FromJSON PlaylistSimplified where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data PlaylistTrack = PlaylistTrack
    { platraAddedAt :: UTCTime
    , platraAddedBy ::  Maybe UserPublic
    , platraIsLocal :: Bool
    , platraTrack :: Track
    } deriving (Generic, Show)

instance FromJSON PlaylistTrack where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Restrictions = Restrictions
    { resReason :: Text
    } deriving (Generic, Show)

instance FromJSON Restrictions where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Track = Track
    { traAlbum :: AlbumSimplified
    , traArtists :: [ArtistSimplified]
    , traAvailableMarkets :: [Text]
    , traDiscNumber :: Integer
    , traDurationMs :: Integer
    , traExplicit :: Bool
    , traExternalIds :: HashMap Text Text
    , traExternalUrls :: HashMap Text Text
    , traHref :: Text
    , traId :: Text
    , traIsPlayable :: Bool
    , traLinkedFrom :: TrackLink
    , traRestrictions :: Restrictions
    , traName :: Text
    , traPopularity :: Integer
    , traPreviewUrl :: Text
    , traTrackNumber :: Integer
    , traType :: Text
    , traUri :: Text
    } deriving (Generic, Show)

instance FromJSON Track where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data TrackLink = TrackLink
    { tralinExternalUrls :: HashMap Text Text
    , tralinHref :: Text
    , tralinId :: Text
    , tralinType :: Text
    , tralinUri :: Text
    } deriving (Generic, Show)

instance FromJSON TrackLink where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase
    
data Tracks = Tracks
    { tracksHref :: Text
    , tracksTotal :: Integer
    } deriving (Generic, Show)

instance FromJSON Tracks where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data UserPrivate = UserPrivate
    { usepriCountry :: Maybe Text
    , usepriDisplayName :: Maybe Text
    , usepriEmail :: Maybe Text
    , usepriExternalUrls :: HashMap Text Text
    , usepriFollowers :: Followers
    , usepriHref :: Text
    , usepriId :: Text
    , usepriImages :: [Image]
    , usepriProduct :: Maybe Text
    , usepriType :: Text
    , usepriUri :: Text
    } deriving (Generic, Show)

instance FromJSON UserPrivate where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data UserPublic = UserPublic
    { usepubDisplayName :: Maybe Text
    , usepubExternalUrls :: HashMap Text Text
    -- , usepubFollowers :: Followers
    , usepubHref :: Text
    , usepubId :: Text
    -- , usepubImages :: [Image]
    , usepubType :: Text
    , usepubUri :: Text
    } deriving (Generic, Show)

instance FromJSON UserPublic where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase
