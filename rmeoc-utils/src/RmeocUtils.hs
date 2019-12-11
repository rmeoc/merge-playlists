{-# LANGUAGE OverloadedStrings         #-}

module RmeocUtils
  ( mergeSequencesBalanced
  ) where

import Data.Maybe     (fromMaybe)
import Data.Monoid    (Ap(..))
import Data.Semigroup (Max(..), mtimesDefault)
import Data.Sequences (IsSequence(..), Index)

mergeSequencesBalanced :: (Applicative m, Functor f, Foldable f, IsSequence seq) => (Index seq -> seq -> m seq) -> f seq -> m seq
mergeSequencesBalanced choose xs =
  fromMaybe (pure mempty) $ do
    Max maxLength <- foldMap (Just . Max . lengthIndex) xs
    return $ getAp $ foldMap (Ap . adjustLength choose maxLength) xs

adjustLength :: (Functor m, IsSequence seq) => (Index seq -> seq -> m seq) -> Index seq -> seq -> m seq
adjustLength choose newLength xs = 
  let
    (numRepetitions, numExtra) = newLength  `divMod` lengthIndex xs
  in
    mappend (numRepetitions `mtimesDefault` xs) <$> choose numExtra xs
