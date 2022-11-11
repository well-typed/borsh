module Codec.Borsh.Incremental.Located (
    -- * Values along with an input location
    ByteOffset
  , Located(..)
  , LocatedChunk
    -- * Located chunks
  , LocatedChunks
  , toLocatedChunks
  , fromLocatedChunks
  , addChunk
  , splitChunks
  ) where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Word

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import qualified Data.List.NonEmpty   as NE

import Codec.Borsh.Internal.Util.ByteString

{-------------------------------------------------------------------------------
 Values along with an input location
-------------------------------------------------------------------------------}

-- | Offset in bytes within the input
type ByteOffset = Word32

-- | Value at a particular point in the input
data Located a = L !a {-# UNPACK #-} !ByteOffset

-- | The most common case: chunk of the input at a particular point
type LocatedChunk = Located S.ByteString

{-------------------------------------------------------------------------------
  Simple application of 'Located' to a bunch of chunks
-------------------------------------------------------------------------------}

-- | Bunch of chunks, starting at a particular point
--
-- The chunks are stored in reverse order, and we cache their total length.
type LocatedChunks = Located (NonEmpty S.ByteString, Word32)

toLocatedChunks :: LocatedChunk -> LocatedChunks
toLocatedChunks (L bs off) = L (bs :| [], lengthStrict bs) off

-- | Concatenate all chunks together
--
-- NOTE: This is expensive, and should be used only in exception circumstances.
fromLocatedChunks :: LocatedChunks -> LocatedChunk
fromLocatedChunks (L (bss, _) off) = L (S.concat (reverse $ toList bss)) off

-- | Add chunk
--
-- This does not affect the offset, since the chunk is (logically) at the /end/
-- of the already-known chunks
addChunk :: S.ByteString -> LocatedChunks -> LocatedChunks
addChunk bs (L (bss, len) off) = L (NE.cons bs bss, len + lengthStrict bs) off

-- | Split chunks at the required length, if sufficient chunks are available
--
-- Precondition: if the accumulated length exceeds the required length, we must
-- be able to split the mostly added chunk to make up for the difference.
splitChunks :: Word32 -> LocatedChunks -> Maybe (L.ByteString, LocatedChunk)
splitChunks reqLen (L (mostRecent :| older, len) off)
  | reqLen > len = Nothing
  | otherwise    = Just (large, L rest (off + fromIntegral (L.length large)))
  where
    excess :: Word32
    excess = len - reqLen

    req, rest :: S.ByteString
    (req, rest) = splitAtEnd (fromIntegral excess) mostRecent

    large :: L.ByteString
    large = L.fromChunks $ reverse (req : older)
