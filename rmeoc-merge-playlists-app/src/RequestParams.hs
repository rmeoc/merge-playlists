{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module RequestParams
    ( RequestParamSerializer(..)
    , RequestParamsSpec
    , requestParam
    , requestParamSpec
    , parseGetParams
    , parsePostParams
    , postButton
    ) where

import Control.Applicative.Free
import Control.Monad.Reader
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Maybe
import Data.Text
import Yesod.Core
import Yesod.Form


data RequestParamsSpecF a = PathPiece a => RequestParamsSpecF Text (Maybe a)

type RequestParamsSpec a = Ap RequestParamsSpecF a

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

requestParam :: PathPiece a => Text -> RequestParamSerializer a
requestParam name = RequestParamSerializer $ \x -> pure (name, toPathPiece x)

requestParamSpec :: PathPiece a => Text -> Maybe a -> RequestParamsSpec a
requestParamSpec name mdef = liftAp $ RequestParamsSpecF name mdef

parseGetParams :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) => RequestParamsSpec a -> m a 
parseGetParams = runInputGet . runAp interpret
    where
        interpret :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => RequestParamsSpecF a -> FormInput m a
        interpret (RequestParamsSpecF name (Just def)) = fromMaybe def <$> iopt hiddenField name
        interpret (RequestParamsSpecF name Nothing) = ireq hiddenField name

parsePostParams :: (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage) => RequestParamsSpec a -> m a
parsePostParams = extractResult <=< runFormPost . const . aFormToForm . runAp interpret
    where
        extractResult :: (MonadHandler m) => ((FormResult a, b), c) -> m a
        extractResult ((formResult, _), _) =
            case formResult of
                FormMissing -> invalidArgs []
                FormFailure messages -> invalidArgs messages
                FormSuccess x -> pure x

        interpret :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) => RequestParamsSpecF a -> AForm m a
        interpret (RequestParamsSpecF name (Just def)) = fromMaybe def <$> aopt hiddenField ("" { fsName = Just name }) Nothing
        interpret (RequestParamsSpecF name Nothing) = areq hiddenField ("" { fsName = Just name }) Nothing

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
