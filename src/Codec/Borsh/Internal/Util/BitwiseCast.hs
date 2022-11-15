module Codec.Borsh.Internal.Util.BitwiseCast (BitwiseCast(..)) where

import Data.Int
import Data.WideWord.Int128
import Data.WideWord.Word128
import Data.Word
import GHC.Float


class BitwiseCast a b where
  -- | Bit-for-bit copy from @a@ to @b@
  castBits :: a -> b

instance BitwiseCast Float  Word32 where castBits = castFloatToWord32
instance BitwiseCast Double Word64 where castBits = castDoubleToWord64

instance BitwiseCast Word32 Float  where castBits = castWord32ToFloat
instance BitwiseCast Word64 Double where castBits = castWord64ToDouble

instance BitwiseCast Word8   Int8   where castBits = fromIntegral
instance BitwiseCast Word16  Int16  where castBits = fromIntegral
instance BitwiseCast Word32  Int32  where castBits = fromIntegral
instance BitwiseCast Word64  Int64  where castBits = fromIntegral
instance BitwiseCast Word128 Int128 where castBits = fromIntegral

instance BitwiseCast Int8   Word8   where castBits = fromIntegral
instance BitwiseCast Int16  Word16  where castBits = fromIntegral
instance BitwiseCast Int32  Word32  where castBits = fromIntegral
instance BitwiseCast Int64  Word64  where castBits = fromIntegral
instance BitwiseCast Int128 Word128 where castBits = fromIntegral
