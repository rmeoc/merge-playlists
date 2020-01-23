{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module RequestParams
    ( RequestParams.Direction(..)
    , RequestParams.Parser
    , field
    , runParserGet
    ) where

import Control.Applicative
import Control.Arrow
import Control.Monad.Reader
import Data.Attoparsec.Text
import Data.Either.Combinators
import Data.Map as Map
import Data.Maybe
import Data.Text as Text
import SpotifyClient
import Yesod.Core


newtype Parser a = Parser (Reader (Map.Map Text [Text]) a) deriving (Applicative, Functor)

runParser :: RequestParams.Parser a -> [(Text,Text)] -> Either [Text] a
runParser (Parser rdr) = Right . runReader rdr . Map.fromListWith (<>) . fmap (second pure)

field :: PathPiece a => Text -> a -> RequestParams.Parser a
field name def = Parser $ reader $ fromMaybe def . parseValues . Map.findWithDefault [] name
    where
        parseValues [x] = fromPathPiece x
        parseValues _ = Nothing

newtype Direction = Direction { toSpotifyClientDirection :: SpotifyClient.Direction }

instance PathPiece RequestParams.Direction where
    fromPathPiece = parseDirection
    toPathPiece = printDirection

parseDirection :: Text -> Maybe RequestParams.Direction
parseDirection =
    fmap Direction . rightToMaybe . parseOnly ((SpotifyClient.Forward <$ string directionForwardText <|> SpotifyClient.Reverse <$ string directionReverseText) <* endOfInput)

printDirection :: RequestParams.Direction -> Text
printDirection (Direction SpotifyClient.Forward) = directionForwardText
printDirection (Direction SpotifyClient.Reverse) = directionReverseText

directionForwardText :: Text
directionForwardText = "forward"

directionReverseText :: Text
directionReverseText = "reverse"

runParserGet :: MonadHandler m => RequestParams.Parser a -> m a
runParserGet p = do
    request <- getRequest
    let parseResult = RequestParams.runParser p $ reqGetParams request
    either invalidArgs pure parseResult
