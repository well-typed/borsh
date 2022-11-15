-- | Swap between big-endian and little endian
--
-- This is adapted from the `memory` package. Once
-- <https://github.com/vincenthz/hs-memory/pull/97> is merged this should not
-- be necessary anymore.
module Codec.Borsh.Internal.Util.ByteSwap (
    ByteSwap(..)
  , LE (..)
  , fromLE
  ) where

import Data.WideWord.Int128
import Data.WideWord.Word128
import Data.Word
import Foreign (Storable)
import GHC.ByteOrder (targetByteOrder, ByteOrder (..))

-- | Little Endian value
newtype LE a = LE { unLE :: a }
  deriving newtype (Show, Eq, Storable)

-- | Convert from a little endian value to the cpu endianness
fromLE :: ByteSwap a => LE a -> a
fromLE (LE a) = if targetByteOrder == LittleEndian then a else byteSwap a

class Storable a => ByteSwap a where
  byteSwap :: a -> a

instance ByteSwap Word8   where byteSwap = id
instance ByteSwap Word16  where byteSwap = byteSwap16
instance ByteSwap Word32  where byteSwap = byteSwap32
instance ByteSwap Word64  where byteSwap = byteSwap64
instance ByteSwap Word128 where byteSwap = byteSwapWord128
instance ByteSwap Int128  where byteSwap = byteSwapInt128
