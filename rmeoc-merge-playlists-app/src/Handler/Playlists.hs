{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Handler.Playlists(getPlaylistsR) where

import Control.Applicative          ((<|>))
import Control.Arrow                (second)
import Control.Monad                ((<=<))
import Control.Monad.Reader         (Reader, ReaderT(..), reader, runReader, runReaderT, withReaderT)
import Data.List                    (sortBy)
import Data.Maybe                   (fromMaybe, listToMaybe)
import Data.Ord                     (Down(..), comparing)
import Data.Text                    (Text)
import Foundation                   (App, Handler, Route(..), Widget, appSpotifyClientContext)
import OAuth2Client                 (getAccessToken)
import UnliftIO                     (MonadUnliftIO)
import Yesod.Core                   (HandlerSite, Html, MonadHandler, defaultLayout, getRequest,
                                     getYesod, liftIO, reqGetParams, whamlet)

import qualified Data.Attoparsec.Text as P
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Network.Wreq.Session as WS
import qualified SpotifyClient as S
import qualified SpotifyClient.Types as S


runSpotify :: (MonadHandler m, MonadUnliftIO m, HandlerSite m ~ App) => ReaderT S.SpotifyClientContext m b -> ReaderT WS.Session m b
runSpotify mx = do
    y <- getYesod
    token <- getAccessToken (appSpotifyClientContext y)
    withReaderT (flip S.SpotifyClientContext token) mx

getPlaylistsR :: Handler Html
getPlaylistsR = do
        pageRef <- queryStringToPageRef . reqGetParams <$> getRequest
        wreqSession <- liftIO WS.newSession
        (mprev, playlists, mnext) <- runReaderT (runSpotify $ S.getPlaylistPage pageRef) wreqSession

        defaultLayout $ 
            [whamlet|
                <h1>Playlists
                $maybe prev <- mprev
                    <p>
                        <a href=@?{playlistsPageRoute prev}>
                            Previous Page
                $maybe next <- mnext
                    <p>
                        <a href=@?{playlistsPageRoute next}>
                            Next Page
                <ul>
                    $forall playlist <- playlists
                        ^{playlistWidget playlist}
            |]
    where
        playlistsPageRoute :: S.PageRef -> (Route App, [(Text, Text)])
        playlistsPageRoute pageRef = (PlaylistsR, pageRefToQueryString pageRef)

        chooseImage :: S.PlaylistSimplified -> Maybe S.Image
        chooseImage = listToMaybe . sortBy compareImages . S.plasimImages

        compareImages :: S.Image -> S.Image -> Ordering
        compareImages =
                comparing
                    (maybe
                        (Right ())
                        ( Left
                        . (\width ->
                            ( Down $ width >= preferredImageWidth
                            , abs $ width - preferredImageWidth
                            )
                        )
                        )
                    .   S.imaWidth
                    )
            where
                preferredImageWidth :: Integer
                preferredImageWidth = 300
        
        playlistWidget :: S.PlaylistSimplified -> Widget
        playlistWidget playlist = do
            [whamlet|
                $with owner <- S.plasimOwner playlist
                    <li>
                        #{S.plasimName playlist}
                        <ul>
                            $maybe image <- chooseImage playlist
                                <li>
                                    <img src=#{S.imaUrl image}>
                            <li> owner: #{fromMaybe (S.usepubId owner) (S.usepubDisplayName owner)}
            |]

fieldDirection :: Text
fieldDirection = "direction"

fieldDirectionForward :: Text
fieldDirectionForward = "forward"

fieldDirectionReverse :: Text
fieldDirectionReverse = "reverse"

fieldOffset :: Text
fieldOffset = "offset"

fieldLimit :: Text
fieldLimit = "limit"

queryStringToPageRef :: [(Text,Text)] -> S.PageRef
queryStringToPageRef = runReader (S.PageRef <$> direction <*> offset <*> limit) . Map.fromListWith (<>) . fmap (second pure)
    where
        singletonItem :: [a] -> Maybe a
        singletonItem [x] = Just x
        singletonItem _ = Nothing

        runParser :: P.Parser a -> Text -> Maybe a
        runParser p = either (const Nothing) Just . P.parseOnly p

        parseDirection :: Text -> Maybe S.Direction
        parseDirection =
            runParser $ (S.Forward <$ P.string fieldDirectionForward <|> S.Reverse <$ P.string fieldDirectionReverse) <* P.endOfInput

        parseInt :: Text -> Maybe Int
        parseInt =
            runParser $ P.signed P.decimal <* P.endOfInput

        field :: Text -> (Text -> Maybe a) -> a -> Reader (Map.Map Text [Text]) a
        field name parse def = reader $ fromMaybe def . (parse <=< singletonItem <=< Map.lookup name)

        direction :: Reader (Map.Map Text [Text]) S.Direction
        direction = field fieldDirection parseDirection S.Forward

        offset :: Reader (Map.Map Text [Text]) Int
        offset = field fieldOffset parseInt 0

        limit :: Reader (Map.Map Text [Text]) Int
        limit = field fieldLimit parseInt 10


pageRefToQueryString :: S.PageRef -> [(Text,Text)]
pageRefToQueryString S.PageRef { S.pageRefDirection, S.pageRefOffset, S.pageRefLimit } =
    [   (fieldDirection, case pageRefDirection of S.Forward -> fieldDirectionForward; S.Reverse -> fieldDirectionReverse)
    ,   (fieldOffset, T.pack $ show pageRefOffset)
    ,   (fieldLimit, T.pack $ show pageRefLimit)
    ]
