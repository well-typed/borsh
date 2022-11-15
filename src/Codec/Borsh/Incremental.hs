-- | Interface for incremental decoding
module Codec.Borsh.Incremental (
    -- * Definition
    Decoder -- Opaque
  , liftDecoder

    -- * Running decoders
  , DeserialiseFailure(..)
  , deserialiseByteString

    -- * Large tokens
  , decodeLargeToken
  , decodeIncremental
  , decodeIncremental_
  ) where

import Codec.Borsh.Incremental.Decoder
import Codec.Borsh.Incremental.Monad
