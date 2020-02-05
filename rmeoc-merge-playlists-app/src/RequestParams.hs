{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}

module RequestParams
    ( RequestParamsSpec
    , requestParamSpec
    , parseGetParams
    , parsePostParams
    , postButton
    ) where

import Control.Applicative.Free
import Control.Arrow
import Control.Monad.Reader
import Data.Maybe
import Data.Text
import Yesod.Core
import Yesod.Form


data RequestParamsSpecF a = PathPiece a => RequestParamsSpecF Text (Maybe a)

type RequestParamsSpec a = Ap RequestParamsSpecF a

requestParamSpec :: PathPiece a => Text -> Maybe a -> RequestParamsSpec a
requestParamSpec name mdef = liftAp $ RequestParamsSpecF name mdef

toFormInput :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => RequestParamsSpec a -> FormInput m a
toFormInput = runAp interpret
    where
        interpret :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => RequestParamsSpecF a -> FormInput m a
        interpret (RequestParamsSpecF name (Just def)) = fromMaybe def <$> iopt hiddenField name
        interpret (RequestParamsSpecF name Nothing) = ireq hiddenField name

parseGetParams :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) => RequestParamsSpec a -> m a 
parseGetParams = runInputGet . toFormInput

fieldSettings :: Text -> FieldSettings site
fieldSettings name
    = FieldSettings
        { fsLabel = SomeMessage name
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Just name
        , fsAttrs = []
        }

fromDiffList :: ([a] -> [a]) -> [a]
fromDiffList diffList = diffList []

formFieldsWidget :: (Functor f, ToWidget site a) => f (d, [FieldView site] -> [FieldView site]) -> a -> f (d, WidgetFor site ())
formFieldsWidget form fragment = second formFieldsWidget' <$> form
    where
        formFieldsWidget' vs = toWidget fragment <> mconcat (fvInput <$> fromDiffList vs)

renderForm ::
    (MonadHandler m, HandlerSite m ~ site, RenderMessage site FormMessage, ToWidget site a) =>
    [(Text,Text)] -> a -> MForm m (FormResult (), WidgetFor site ())
renderForm = formFieldsWidget . aFormToForm . foldMap toAForm
    where
        toAForm :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) => (Text,Text) -> AForm m ()
        toAForm (name, value) = void $ areq hiddenField (fieldSettings name) (Just value)

postButton :: (RenderMessage site FormMessage) => Route site -> [(Text,Text)] -> Text -> WidgetFor site ()
postButton route params buttonText = do
    (widget, enctype) <- generateFormPost $ renderForm params
    [whamlet|
        <form method=post action=@{route} enctype=#{enctype}>
            ^{widget}
            <button>#{buttonText}
    |]

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
        interpret (RequestParamsSpecF name (Just def)) = fromMaybe def <$> aopt hiddenField (fieldSettings name) Nothing
        interpret (RequestParamsSpecF name Nothing) = areq hiddenField (fieldSettings name) Nothing
