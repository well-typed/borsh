{-# LANGUAGE PolyKinds #-}

module Codec.Borsh.Internal.Util.SOP (
    indices
  ) where

import Data.SOP
import Data.Word

indices :: forall k (xs :: [k]). SListI xs => NP (K Word8) xs
indices = go sList 0
  where
    go :: forall xs'. SList xs' -> Word8 -> NP (K Word8) xs'
    go SNil  _  = Nil
    go SCons ix = K ix :* go sList (succ ix)
