module Codec.Borsh.Internal.Util.ByteString (
    peekByteString
  , splitAtEnd
  , lengthStrict
  , lengthLazy
  ) where

import Foreign
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString          as S
import qualified Data.ByteString.Internal as S.Internal
import qualified Data.ByteString.Lazy     as L

import Codec.Borsh.Internal.Util.ByteSwap

-- | Peek at the start of the bytestring
--
-- If the bytestring is long enough, returns the value, the size of that value,
-- and the remaining bytes.
--
-- Implementation note: this could be simplified using @bytestring >= 0.11@, as
-- the @offset@ argument has been removed. As it stands, this implementation is
-- backwards compatible.
peekByteString :: forall a.
     ByteSwap a
  => S.ByteString
  -> Maybe (a, Word32, S.ByteString)
peekByteString bs
  | sizeA > len = Nothing
  | otherwise   = Just (
        fromLE . LE $ unsafePerformIO $ withForeignPtr (plusForeignPtr fPtr offset) (peek . cast)
      , fromIntegral sizeA
      , S.drop sizeA bs
      )
  where
    sizeA :: Int
    sizeA = sizeOf (undefined :: a)

    fPtr :: ForeignPtr Word8
    offset, len :: Int
    (fPtr, offset, len) = S.Internal.toForeignPtr bs

    cast :: Ptr Word8 -> Ptr a
    cast = castPtr

-- | /O(1)/ @splitAtEnd n xs@ is equivalent to @(takeEnd n xs, dropEnd n xs)@
--
-- > splitAtEnd 0 "abcde" == ("abcde", "")
-- > splitAtEnd 1 "abcde" == ("abcd", "e")
-- > splitAtEnd 5 "abcde" == ("", "abcde")
--
-- Edge cases, similar to behaviour of 'splitAt':

-- > splitAtEnd (-1) "abcde" == ("abcde", "") -- split before start
-- > splitAtEnd 6    "abcde" == ("", "abcde") -- split after end
splitAtEnd ::
     Int
  -> S.ByteString
  -> (S.ByteString, S.ByteString)
splitAtEnd n bs = S.splitAt n' bs
  where
    -- This may drop below zero if @n > length bs@. This will give us the
    -- correct behaviour from 'splitAt'
    n' :: Int
    n' = S.length bs - n

-- | Wrapper around 'S.length' with more sane return type
lengthStrict :: S.ByteString -> Word32
lengthStrict = fromIntegral . S.length

-- | Wrapper around 'L.length' with more sane return type
lengthLazy :: L.ByteString -> Word32
lengthLazy = fromIntegral . L.length