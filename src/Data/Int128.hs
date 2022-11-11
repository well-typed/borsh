{-# LANGUAGE DeriveDataTypeable #-}

module Data.Int128 (
    -- * Definition
    Int128 -- Opaque
    -- * Construction
  , int128
    -- * Destruction
  , int128MS64
  , int128LS64
  ) where

import Data.Bits
import Data.Data
import Data.Ix
import Data.Word
import Foreign
import GHC.Generics

import qualified Data.WideWord.Int128 as WW

import Codec.Borsh.Internal.Util.ByteSwap (ByteSwap(..))

{-------------------------------------------------------------------------------
  Definition, construction, destruction
-------------------------------------------------------------------------------}

-- | Signed 128-bit word
--
-- Implementation note: this currently relies on the implementation of the
-- [wide-word](https://hackage.haskell.org/package/wide-word) package, with some
-- additional instances. However, the use of @wide-word@ is not part of the
-- public API of the @borsh@ package.
newtype Int128 = Int128 WW.Int128
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

-- | Construct an 'Int128'
int128 ::
     Word64 -- ^ Most significant bits
  -> Word64 -- ^ Least significant bits
  -> Int128
int128 hi lo = Int128 (WW.Int128 hi lo)

-- | Get the most significant 64 bits from an 'Int128'
int128MS64 :: Int128 -> Word64
int128MS64 (Int128 (WW.Int128 hi _)) = hi

-- | Get the least significant 64 bits from an 'Int128'
int128LS64 :: Int128 -> Word64
int128LS64 (Int128 (WW.Int128 _ lo)) = lo

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance ByteSwap Int128 where
  byteSwap (Int128 (WW.Int128 hi lo)) =
      Int128 $ WW.Int128 (byteSwap64 lo) (byteSwap64 hi)
