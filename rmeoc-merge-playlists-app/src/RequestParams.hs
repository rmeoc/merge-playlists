{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module RequestParams
    ( RequestParams.Parser
    , field
    , runParserGet
    ) where

import Control.Applicative.Free
import Control.Arrow
import Control.Monad.Reader
import Data.Map as Map
import Data.Text
import Yesod.Core


data ParserF a = PathPiece a => ParserF Text (Maybe a)

type Parser a = Ap ParserF a

runParser :: RequestParams.Parser a -> [(Text,Text)] -> Either [Text] a
runParser p params = runReaderT (runAp toReader p) paramsMap
    where
        paramsMap :: Map Text [Text]
        paramsMap = Map.fromListWith (<>) $ fmap (second pure) params

        toReader :: RequestParams.ParserF a -> ReaderT (Map Text [Text]) (Either [Text]) a
        toReader (ParserF name mdef) = ReaderT $ left adjustError . parseValues . Map.findWithDefault [] name
            where
                adjustError :: Text -> [Text]
                adjustError err = ["Failed to parse \"" <> name <> "\": " <> err]

                parseValues [] = maybe (Left "missing value") Right mdef
                parseValues [x] = maybe (Left $ "invalid value: " <> x) Right $ fromPathPiece x
                parseValues xs = Left ("multiple values: " <> intercalate ", " xs)

field :: PathPiece a => Text -> Maybe a -> RequestParams.Parser a
field name mdef = liftAp $ ParserF name mdef

runParserGet :: MonadHandler m => RequestParams.Parser a -> m a
runParserGet p = do
    request <- getRequest
    let parseResult = RequestParams.runParser p $ reqGetParams request
    either invalidArgs pure parseResult
