{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
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
import Data.Maybe
import Data.Text
import Yesod.Core
import Yesod.Form


data FormSpecF a = PathPiece a => FormSpecF Text (Maybe a)

type FormSpec a = Ap FormSpecF a

field :: PathPiece a => Text -> Maybe a -> FormSpec a
field name mdef = liftAp $ FormSpecF name mdef

toFormInput :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => FormSpec a -> FormInput m a
toFormInput = runAp interpret
    where
        interpret :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => FormSpecF a -> FormInput m a
        interpret (FormSpecF name (Just def)) = fromMaybe def <$> iopt hiddenField name
        interpret (FormSpecF name Nothing) = ireq hiddenField name

parseFormGet :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) => FormSpec a -> m a 
parseFormGet = runInputGet . toFormInput
