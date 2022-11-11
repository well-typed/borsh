{-# LANGUAGE DeriveDataTypeable #-}

module Data.Word128 (
    -- * Definition
    Word128 -- opaque
    -- * Construction
  , word128
    -- * Destruction
  , word128MS64
  , word128LS64
  ) where

import Data.Bits
import Data.Data
import Data.Ix
import Data.Word
import Foreign
import GHC.Generics

import qualified Data.WideWord.Word128 as WW

import Codec.Borsh.Internal.Util.ByteSwap (ByteSwap(..))

{-------------------------------------------------------------------------------
  Definition, construction, destruction
-------------------------------------------------------------------------------}

-- | Unsigned 128-bit word
--
-- Implementation note: this currently relies on the implementation of the
-- [wide-word](https://hackage.haskell.org/package/wide-word) package, with some
-- additional instances. However, the use of @wide-word@ is not part of the
-- public API of the @borsh@ package.
newtype Word128 = Word128 WW.Word128
  deriving stock Data
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Generic
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | Construct a 'Word128'
word128 ::
     Word64 -- ^ Most significant bits
  -> Word64 -- ^ Least significant bits
  -> Word128
word128 hi lo = Word128 (WW.Word128 hi lo)

-- | Get the most significant 64 bits from a 'Word128'
word128MS64 :: Word128 -> Word64
word128MS64 (Word128 (WW.Word128 hi _)) = hi

-- | Get the least significant 64 bits from a 'Word128'
word128LS64 :: Word128 -> Word64
word128LS64 (Word128 (WW.Word128 _ lo)) = lo

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance ByteSwap Word128 where
  byteSwap (Word128 (WW.Word128 hi lo)) =
      Word128 $ WW.Word128 (byteSwap64 lo) (byteSwap64 hi)
