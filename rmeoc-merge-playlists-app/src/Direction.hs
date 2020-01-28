{-# LANGUAGE OverloadedStrings          #-}

module Direction
    ( Direction(..)
    ) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Either.Combinators
import Data.Text
import qualified SpotifyClient
import Yesod.Core

newtype Direction = Direction { toSpotifyClientDirection :: SpotifyClient.Direction }

instance PathPiece Direction where
    fromPathPiece = parseDirection
    toPathPiece = printDirection

parseDirection :: Text -> Maybe Direction
parseDirection =
    fmap Direction . rightToMaybe . parseOnly ((SpotifyClient.Forward <$ string directionForwardText <|> SpotifyClient.Reverse <$ string directionReverseText) <* endOfInput)

printDirection :: Direction -> Text
printDirection (Direction SpotifyClient.Forward) = directionForwardText
printDirection (Direction SpotifyClient.Reverse) = directionReverseText

directionForwardText :: Text
directionForwardText = "forward"

directionReverseText :: Text
directionReverseText = "reverse"
