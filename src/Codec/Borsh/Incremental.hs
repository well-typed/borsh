
module Codec.Borsh.Incremental (
    -- * Constructing decoders
    Decoder(..)
  , DecodeResult(..)
  , liftDecoder
    -- * Running decoders
  , DeserialiseFailure(..)
  , deserialiseByteString
    -- * Specialised decoders
    --
    -- | These functions comprise a low-level decoder interface which will not
    -- be necessary for most applications. Most applications should simply use
    -- 'Codec.Borsh.Class.deserialiseBorsh'
  , decodeLargeToken
  , decodeIncremental
  , decodeIncremental_
    -- * Located values
  , Located(..)
  , ByteOffset
  , LocatedChunk
  ) where

import Codec.Borsh.Incremental.Decoder
import Codec.Borsh.Incremental.Located
import Codec.Borsh.Incremental.Monad
