module Codec.Borsh.Incremental.Monad (
    -- * Definition
    Incr(..)
  , runIncr
    -- * Operations supported by the monad
  , liftIncr
  , needChunk
  , decodeFail
    -- * (Partial) results
  , IDecode(..)
  , DeserialiseFailure(..)
  , runIDecode
  ) where

import Control.Monad
import Control.Monad.ST
import Control.Exception

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

import Codec.Borsh.Incremental.Located

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Monad for incremental decoding
--
-- Think of 'Incr' as the monad we use for processing the full input, whereas
-- 'Decoder' is the monad used for processing a single chunk of the input.
newtype Incr s a = Incr {
      getIncr :: forall r. (a -> ST s (IDecode s r)) -> ST s (IDecode s r)
    }

runIncr :: Incr s (LocatedChunk, a) -> ST s (IDecode s a)
runIncr (Incr f) = f $ \(chunk, x) -> return $ IDecodeDone chunk x

{-------------------------------------------------------------------------------
  Monad instance
-------------------------------------------------------------------------------}

instance Functor (Incr s) where
  fmap = liftM

instance Applicative (Incr s) where
  pure x = Incr ($ x)
  (<*>)  = ap

instance Monad (Incr s) where
  return  = pure
  m >>= f = Incr $ \k -> getIncr m $ \x -> getIncr (f x) k

{-------------------------------------------------------------------------------
  Operations supported by the monad
-------------------------------------------------------------------------------}

liftIncr :: ST s a -> Incr s a
liftIncr action = Incr (action >>=)

needChunk :: Incr s (Maybe S.ByteString)
needChunk = Incr $ \k -> return $ IDecodePartial $ \mbs -> k mbs

decodeFail :: LocatedChunk -> String -> Incr s a
decodeFail chunk@(L _ off) e = Incr $ \_ ->
    return $ IDecodeFail chunk (DeserialiseFailure off e)

{-------------------------------------------------------------------------------
  (Partial) results
-------------------------------------------------------------------------------}

data IDecode s a =
    IDecodePartial (Maybe S.ByteString -> ST s (IDecode s a))
  | IDecodeDone !LocatedChunk a
  | IDecodeFail !LocatedChunk DeserialiseFailure

-- | Error type for deserialisation.
data DeserialiseFailure =
    DeserialiseFailure
      ByteOffset -- ^ The position of the decoder when the failure occurred
      String     -- ^ Message explaining the failure
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

runIDecode ::
     (forall s. ST s (IDecode s a))
  -> L.ByteString
  -> Either DeserialiseFailure (L.ByteString, ByteOffset, a)
runIDecode d lbs =
    runST (go (L.toChunks lbs) =<< d)
  where
    go :: [S.ByteString]
       -> IDecode s a
       -> ST s (Either DeserialiseFailure (L.ByteString, ByteOffset, a))
    go chunks = \case
        IDecodeFail _ err ->
          return (Left err)
        IDecodeDone (L bs off) x ->
          return (Right (L.fromChunks $ prepend bs chunks, off, x))
        IDecodePartial k ->
          case chunks of
            []         -> k Nothing   >>= go []
            bs:chunks' -> k (Just bs) >>= go chunks'

    prepend :: S.ByteString -> [S.ByteString] -> [S.ByteString]
    prepend bs bss
      | S.null bs = bss
      | otherwise = bs : bss

