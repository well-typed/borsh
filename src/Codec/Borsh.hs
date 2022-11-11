module Codec.Borsh (
    -- * Serialisation
    ToBorsh(..)
  , Encoder(..)
  , serialiseBorsh
    -- * Deserialisation
  , FromBorsh(..)
  , Decoder
  , DeserialiseFailure(..)
  , deserialiseBorsh
    -- * Size of encodings
  , BorshSize(..)
  , Size(..)
  , KnownSize(..)
  , BorshSizeSum(..)
    -- * Deriving-via support
  , Struct(..)
  ) where

import Codec.Borsh.Class
    ( BorshSizeSum(..),
      Struct(..),
      FromBorsh(..),
      ToBorsh(..),
      BorshSize(..),
      Size(..),
      KnownSize(..),
      serialiseBorsh,
      deserialiseBorsh )
import Codec.Borsh.Encoding (Encoder(..))
import Codec.Borsh.Incremental (Decoder)
import Codec.Borsh.Incremental.Monad (DeserialiseFailure(..))
