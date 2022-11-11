-- | Swap between big-endian and little endian
--
-- This is adapted from the `memory` package. Once
-- <https://github.com/vincenthz/hs-memory/pull/97> is merged this should not
-- be necessary anymore.
module Codec.Borsh.Internal.Util.ByteSwap (
    ByteSwap(..)
  , LE(..)
  , toLE
  , fromLE
  , BE(..)
  , toBE
  , fromBE
  ) where

import Data.Word
import Foreign (Storable)

import Data.Memory.Endian (getSystemEndianness, Endianness(LittleEndian))

-- | Little Endian value
newtype LE a = LE { unLE :: a }
  deriving newtype (Show, Eq, Storable)

-- | Big Endian value
newtype BE a = BE { unBE :: a }
  deriving newtype (Show, Eq, Storable)

-- | Convert a value in cpu endianess to big endian
toBE :: ByteSwap a => a -> BE a
toBE = BE . (if getSystemEndianness == LittleEndian then byteSwap else id)

-- | Convert from a big endian value to the cpu endianness
fromBE :: ByteSwap a => BE a -> a
fromBE (BE a) = if getSystemEndianness == LittleEndian then byteSwap a else a

-- | Convert a value in cpu endianess to little endian
toLE :: ByteSwap a => a -> LE a
toLE = LE . (if getSystemEndianness == LittleEndian then id else byteSwap)

-- | Convert from a little endian value to the cpu endianness
fromLE :: ByteSwap a => LE a -> a
fromLE (LE a) = if getSystemEndianness == LittleEndian then a else byteSwap a

class Storable a => ByteSwap a where
  byteSwap :: a -> a

instance ByteSwap Word8  where byteSwap = id
instance ByteSwap Word16 where byteSwap = byteSwap16
instance ByteSwap Word32 where byteSwap = byteSwap32
instance ByteSwap Word64 where byteSwap = byteSwap64
