{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings         #-}

module RequestParams
    ( RequestParams.Parser
    , field
    , parseDirection
    , parseInt
    , printDirection
    , printInt
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

runParser :: RequestParams.Parser a -> [(Text,Text)] -> a
runParser (Parser rdr) = runReader rdr . Map.fromListWith (<>) . fmap (second pure)

field :: (Text -> Maybe a) -> Text -> a -> RequestParams.Parser a
field parseValue name def = Parser $ reader $ fromMaybe def . parseValues . Map.findWithDefault [] name
    where
        parseValues [x] = parseValue x
        parseValues _ = Nothing

parseDirection :: Text -> Maybe SpotifyClient.Direction
parseDirection =
    rightToMaybe . parseOnly ((SpotifyClient.Forward <$ string directionForwardText <|> SpotifyClient.Reverse <$ string directionReverseText) <* endOfInput)

printDirection :: SpotifyClient.Direction -> Text
printDirection SpotifyClient.Forward = directionForwardText
printDirection SpotifyClient.Reverse = directionReverseText

directionForwardText :: Text
directionForwardText = "forward"

directionReverseText :: Text
directionReverseText = "reverse"

parseInt :: Text -> Maybe Int
parseInt =
    rightToMaybe . parseOnly (signed decimal <* endOfInput)

printInt :: Int -> Text
printInt = Text.pack . show

runParserGet :: MonadHandler m => RequestParams.Parser a -> m a
runParserGet p = RequestParams.runParser p . reqGetParams <$> getRequest
