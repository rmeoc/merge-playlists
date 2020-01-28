{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module RequestParams
    ( FormSpec
    , field
    , parseFormGet
    ) where

import Control.Applicative.Free
import Control.Arrow
import Control.Monad.Reader
import Data.Map as Map
import Data.Text
import Yesod.Core


data FormSpecF a = PathPiece a => FormSpecF Text (Maybe a)

type FormSpec a = Ap FormSpecF a

parseForm :: FormSpec a -> [(Text,Text)] -> Either [Text] a
parseForm p params = runReaderT (runAp toReader p) paramsMap
    where
        paramsMap :: Map Text [Text]
        paramsMap = Map.fromListWith (<>) $ fmap (second pure) params

        toReader :: FormSpecF a -> ReaderT (Map Text [Text]) (Either [Text]) a
        toReader (FormSpecF name mdef) = ReaderT $ left adjustError . parseValues . Map.findWithDefault [] name
            where
                adjustError :: Text -> [Text]
                adjustError err = ["Failed to parse \"" <> name <> "\": " <> err]

                parseValues [] = maybe (Left "missing value") Right mdef
                parseValues [x] = maybe (Left $ "invalid value: " <> x) Right $ fromPathPiece x
                parseValues xs = Left ("multiple values: " <> intercalate ", " xs)

field :: PathPiece a => Text -> Maybe a -> FormSpec a
field name mdef = liftAp $ FormSpecF name mdef

parseFormGet :: MonadHandler m => FormSpec a -> m a
parseFormGet p = do
    request <- getRequest
    let parseResult = parseForm p $ reqGetParams request
    either invalidArgs pure parseResult
