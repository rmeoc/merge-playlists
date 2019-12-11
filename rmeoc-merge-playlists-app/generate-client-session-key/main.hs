{-# LANGUAGE NoImplicitPrelude #-}

import ClassyPrelude

import Web.ClientSession (randomKey)

import qualified Data.ByteString as BS


main :: IO ()
main = do
    (bs, _) <- randomKey
    BS.putStr bs
