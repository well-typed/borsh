{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Codec.Borsh.Incremental.Decoder (
    -- * Definition
    Decoder(..)
    -- * Operations supported by any decoder
  , liftDecoder
  , decodeLittleEndian
  , decodeLargeToken
  , decodeIncremental
  , decodeIncremental_
    -- * Running
  , DecodeResult(..)
  , deserialiseByteString
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Control.Monad.ST
import Data.Word

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

import Codec.Borsh.Incremental.Located
import Codec.Borsh.Incremental.Monad
import Codec.Borsh.Internal.Util.ByteString
import Codec.Borsh.Internal.Util.ByteSwap

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Decoder
--
-- A decoder describes how to match against a single chunk of the input.
newtype Decoder s a = Decoder {
      matchChunk :: LocatedChunk -> ST s (LocatedChunk, DecodeResult s a)
    }

{-------------------------------------------------------------------------------
  Operations supported by the 'Decoder' monad
-------------------------------------------------------------------------------}

liftDecoder :: ST s a -> Decoder s a
liftDecoder sa = Decoder $ \chunk -> (chunk, ) . DecodeDone <$> sa

decodeLittleEndian :: forall s a. ByteSwap a => Decoder s a
decodeLittleEndian = Decoder aux
  where
    aux :: LocatedChunk -> ST s (LocatedChunk, DecodeResult s a)
    aux chunk@(L bs off) =
        case peekByteString bs of
          Just (x, sizeX, bs') ->
            return (L bs' (off + sizeX), DecodeDone x)
          Nothing ->
            return (chunk, DecodeNeedsData decodeLittleEndian)

decodeLargeToken :: Word32 -> Decoder s L.ByteString
decodeLargeToken n = Decoder $ \chunk ->
    return (chunk, DecodeLargeToken n return)

decodeIncremental :: Word32 -> Decoder s a -> Decoder s [a]
decodeIncremental n d = Decoder $ \chunk ->
    return (chunk, DecodeIncremental n d return)

decodeIncremental_ :: Word32 -> Decoder s () -> Decoder s ()
decodeIncremental_ n d = Decoder $ \chunk ->
    return (chunk, DecodeIncremental_ n d $ return ())

{-------------------------------------------------------------------------------
  Results
-------------------------------------------------------------------------------}

data DecodeResult s a where
  -- | The decoder terminated successfully: we can stop decoding
  DecodeDone :: a -> DecodeResult s a

  -- | The decoder failed: we should abort
  DecodeFail :: String -> DecodeResult s a

  -- | The decoder needs more data before it can continue
  --
  -- NOTE: The decoder that is waiting for more data may not be (and typically
  -- will not be) the decoder we started with in 'matchChunk': in the typical
  -- case, a bunch of values will have been decoded successfully before we get
  -- to a (continuation) decoder that requires data beyond the current chunk.
  DecodeNeedsData :: Decoder s a -> DecodeResult s a

  -- | Large token of known length that spans multiple chunks
  --
  -- This is NOT incremental: all chunks will be read into memory before the
  -- function is applied. Primarily useful for large types that are not
  -- easily split into (valid) chunks, such as UTF8-encoded text (if were
  -- wanted to split that, we'd have to split it at UTF8 boundaries).
  --
  -- The continuation will be called with a lazy bytestring of precisely the
  -- requested length (provided enough input is available, of course), along
  -- with the remaining input token to be provided to the continuation decoder.
  DecodeLargeToken ::
       Word32  -- ^ Required number of bytes
    -> (L.ByteString -> Decoder s a)
    -> DecodeResult s a

  -- | Incremental interface
  --
  -- When decoding large objects such as lists, we do not want to bring all
  -- required chunks into memory before decoding the list. Instead, we want to
  -- decode the list elements as we go. In this case, 'DecodeIncremental' can
  -- be used to repeatedly decode a value using decoder for the elements; when
  -- all elements have been processed, the continuation decoder is called.
  --
  -- NOTE: This interface is incremental in the sense that the /input chunks/
  -- are read one at a time. It is /NOT/ incremental in the generated /output/.
  DecodeIncremental ::
       Word32               -- ^ How often to repeat the smaller decoder
    -> Decoder s a          -- ^ Decoder to repeat
    -> ([a] -> Decoder s b) -- ^ Process all elements
    -> DecodeResult s b

  -- | Variation on 'DecodeIncremental', where we do not accumulate results
  --
  -- This is useful for example for datatypes that we can update imperatively,
  -- such as mutable arrays. It could also be used to skip over unused parts
  -- of the input.
  DecodeIncremental_ ::
       Word32        -- ^ How often to repeat the smaller decoder
    -> Decoder s ()  -- ^ Decoder to repeat (imperatively handling each element)
    -> Decoder s a   -- ^ Continuation
    -> DecodeResult s a

{-------------------------------------------------------------------------------
  Monad instance
-------------------------------------------------------------------------------}

instance Functor (Decoder s) where
  fmap = liftA

instance Applicative (Decoder s) where
  pure x = Decoder $ \chunk -> return (chunk, DecodeDone x)
  (<*>)  = ap

instance Monad (Decoder s) where
  return  = pure
  x >>= f = Decoder $ \chunk -> do
      (chunk', result) <- matchChunk x chunk
      case result of
        DecodeDone a ->
          matchChunk (f a) chunk'
        DecodeFail e ->
          return (chunk', DecodeFail e)
        DecodeNeedsData d ->
          return (chunk', DecodeNeedsData (d >>= f))
        DecodeLargeToken reqLen k ->
          return (chunk', DecodeLargeToken reqLen (k >=> f))
        DecodeIncremental count d k ->
          return (chunk', DecodeIncremental count d (k >=> f))
        DecodeIncremental_ count d k ->
          return (chunk', DecodeIncremental_ count d (k >>= f))

instance MonadFail (Decoder s) where
  fail e = Decoder $ \chunk -> return (chunk, DecodeFail e)

{-------------------------------------------------------------------------------
  Running decoders
-------------------------------------------------------------------------------}

-- | Top-level entry point
--
-- We start without any input at all (and depending on the specific decoder,
-- we may never need any).
runDecoder :: Decoder s a -> Incr s (LocatedChunk, a)
runDecoder = runWith $ L S.empty 0

-- | Run decoder against specified chunk
runWith :: LocatedChunk -> Decoder s a -> Incr s (LocatedChunk, a)
runWith chunk d = uncurry processResult =<< liftIncr (matchChunk d chunk)

{-------------------------------------------------------------------------------
  Processing the result of a decoder
-------------------------------------------------------------------------------}

-- | Process decoder result
processResult :: LocatedChunk -> DecodeResult s a -> Incr s (LocatedChunk, a)
processResult chunk = \case
    DecodeDone x -> return    (chunk, x)
    DecodeFail e -> decodeFail chunk  e

    DecodeNeedsData      d   -> processNeedsData      d   chunk
    DecodeLargeToken   n   k -> processLargeToken   n   k chunk
    DecodeIncremental  n d k -> processIncremental  n d k chunk
    DecodeIncremental_ n d k -> processIncremental_ n d k chunk

processNeedsData ::
     Decoder s a
  -> Located S.ByteString
  -> Incr s (LocatedChunk, a)
processNeedsData d chunk@(L bs off) = needChunk >>= \case
    Nothing   -> decodeFail chunk "end of input"
    Just next -> runWith (L (bs <> next) off) d

-- | Auxiliary to 'processResult': process token that spans multple chunks
--
-- Precondition: if the accumulated length exceeds the required length, we must
-- be able to split the mostly added chunk to make up for the difference.
processLargeToken :: forall s a.
     Word32                         -- ^ Required total size
  -> (L.ByteString -> Decoder s a)  -- ^ Continuation
  -> LocatedChunk                   -- ^ Current chunk
  -> Incr s (LocatedChunk, a)
processLargeToken reqLen k = go . toLocatedChunks
  where
    go :: LocatedChunks -> Incr s (LocatedChunk, a)
    go acc =
        case splitChunks reqLen acc of
          Nothing -> needChunk >>= \case
            Nothing   -> decodeFail (fromLocatedChunks acc) "end of input"
            Just next -> go (addChunk next acc)
          Just (large, left) ->
            uncurry processResult =<< liftIncr (matchChunk (k large) left)

-- | Auxiliary to 'processResult': incremental decoding
processIncremental :: forall s a b.
     Word32                -- ^ Number of elements required
  -> Decoder s a           -- ^ Decoder to repeat
  -> ([a] -> Decoder s b)  -- ^ Continuation once we processed all elements
  -> LocatedChunk          -- ^ Current chunk
  -> Incr s (LocatedChunk, b)
processIncremental count d k = go [] count
  where
    go :: [a] -> Word32 -> LocatedChunk -> Incr s (LocatedChunk, b)
    go acc 0 chunk = do result <- liftIncr (matchChunk (k (reverse acc)) chunk)
                        uncurry processResult result
    go acc n chunk = do (chunk', a) <- runWith chunk d
                        go (a:acc) (n - 1) chunk'

-- | Imperative version of 'processIncremental'
--
-- See 'DecodeIncremental_' for discussion.
processIncremental_ :: forall s a.
     Word32        -- ^ Number of elements required
  -> Decoder s ()  -- ^ Decoder to repeat
  -> Decoder s a   -- ^ Continuation once we processed all elements
  -> LocatedChunk  -- ^ Current chunk
  -> Incr s (LocatedChunk, a)
processIncremental_ count d k = go count
  where
    go :: Word32 -> LocatedChunk -> Incr s (LocatedChunk, a)
    go 0 chunk = do result <- liftIncr (matchChunk k chunk)
                    uncurry processResult result
    go n chunk = do (chunk', ()) <- runWith chunk d
                    go (n - 1) chunk'

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

deserialiseByteString ::
     (forall s. Decoder s a)
  -> L.ByteString
  -> Either DeserialiseFailure (L.ByteString, ByteOffset, a)
deserialiseByteString d = runIDecode (runIncr (runDecoder d))
