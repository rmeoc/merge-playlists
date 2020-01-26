{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module RequestParams
    ( RequestParams.Direction(..)
    , RequestParams.Parser
    , field
    , runParserGet
    ) where

import Control.Applicative
import Control.Applicative.Free
import Control.Arrow
import Control.Monad.Reader
import Data.Attoparsec.Text
import Data.Either.Combinators
import Data.Map as Map
import Data.Text as Text
import SpotifyClient
import Yesod.Core

data ParserF a = PathPiece a => ParserF Text (Maybe a)

type Parser a = Ap ParserF a

runParser :: RequestParams.Parser a -> [(Text,Text)] -> Either [Text] a
runParser p params = runReaderT (runAp toReader p) paramsMap
    where
        paramsMap :: Map.Map Text [Text]
        paramsMap = Map.fromListWith (<>) $ fmap (second pure) params

        toReader :: RequestParams.ParserF a -> ReaderT (Map.Map Text [Text]) (Either [Text]) a
        toReader (ParserF name mdef) = ReaderT $ left adjustError . parseValues . Map.findWithDefault [] name
            where
                adjustError :: Text -> [Text]
                adjustError err = ["Failed to parse \"" <> name <> "\": " <> err]

                parseValues [] = maybe (Left "missing value") Right mdef
                parseValues [x] = maybe (Left $ "invalid value: " <> x) Right $ fromPathPiece x
                parseValues xs = Left ("multiple values: " <> intercalate ", " xs)

field :: PathPiece a => Text -> Maybe a -> RequestParams.Parser a
field name mdef = liftAp $ ParserF name mdef

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
