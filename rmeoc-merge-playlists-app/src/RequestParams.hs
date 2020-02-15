{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module RequestParams
    ( RequestParamsSpec
    , requestParam
    , parseGetParams
    , parsePostParams
    , postButton
    , toRequestParams
    ) where

import Control.Applicative.Free
import Control.Invertible.Monoidal
import Control.Monad.Reader
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import qualified Data.Invertible as Inv
import Data.Maybe
import Data.Text
import Yesod.Core
import Yesod.Form


data RequestParamsParserF a = PathPiece a => RequestParamsParserF Text (Maybe a)

type RequestParamsParser a = Ap RequestParamsParserF a

newtype RequestParamSerializer a = RequestParamSerializer { runRequestParamSerializer :: a -> [(Text,Text)] }

instance Contravariant RequestParamSerializer where
  contramap f s = RequestParamSerializer (runRequestParamSerializer s . f)

instance Divisible RequestParamSerializer where
  conquer = RequestParamSerializer (const mempty)
  divide toBC bSerializer cSerializer = RequestParamSerializer $ \a ->
    case toBC a of
      (b, c) ->
        let bParams = runRequestParamSerializer bSerializer b
            cParams = runRequestParamSerializer cSerializer c
        in bParams <> cParams

data RequestParamsSpec a = RequestParamsSpec { rpsSerializer :: RequestParamSerializer a, rpsParser :: RequestParamsParser a }

instance Inv.Functor RequestParamsSpec where
    fmap f (RequestParamsSpec serializer parser) = RequestParamsSpec ((biFrom f) `contramap` serializer) (biTo f <$> parser)

instance Monoidal RequestParamsSpec where
    unit = RequestParamsSpec conquer (pure ())
    (>*<) (RequestParamsSpec serializer1 parser1) (RequestParamsSpec serializer2 parser2)
        = RequestParamsSpec (divided serializer1 serializer2) ((,) <$> parser1 <*> parser2)

requestParam :: PathPiece a => Text -> Maybe a -> RequestParamsSpec a
requestParam name mdef =
    RequestParamsSpec
        (RequestParamSerializer $ \x -> pure (name, toPathPiece x))
        (liftAp $ RequestParamsParserF name mdef)

parseGetParams :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) => RequestParamsSpec a -> m a 
parseGetParams = runInputGet . runAp interpret . rpsParser
    where
        interpret :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => RequestParamsParserF a -> FormInput m a
        interpret (RequestParamsParserF name (Just def)) = fromMaybe def <$> iopt hiddenField name
        interpret (RequestParamsParserF name Nothing) = ireq hiddenField name

parsePostParams :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage) => RequestParamsSpec a -> m a
parsePostParams = extractResult <=< runFormPost . const . aFormToForm . runAp interpret . rpsParser
    where
        extractResult :: (MonadHandler m) => ((FormResult a, b), c) -> m a
        extractResult ((formResult, _), _) =
            case formResult of
                FormMissing -> invalidArgs []
                FormFailure messages -> invalidArgs messages
                FormSuccess x -> pure x

        interpret :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) => RequestParamsParserF a -> AForm m a
        interpret (RequestParamsParserF name (Just def)) = fromMaybe def <$> aopt hiddenField ("" { fsName = Just name }) Nothing
        interpret (RequestParamsParserF name Nothing) = areq hiddenField ("" { fsName = Just name }) Nothing

toRequestParams :: RequestParamsSpec a -> a -> [(Text,Text)]
toRequestParams = runRequestParamSerializer . rpsSerializer

postButton :: (RenderMessage site FormMessage) => Route site -> [(Text,Text)] -> Text -> WidgetFor site ()
postButton route params text = do
        (token, enctype) <- generateFormPost $ pure . (pure (),)
        [whamlet|
            <form method=post action=@{route} enctype=#{enctype}>
                #{token}
                $forall (name, value) <- params
                    <input type=hidden name=#{name} value=#{value}>
                <button>#{text}
        |]
