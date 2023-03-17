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
  , BorshMaxSize(..)
    -- * Deriving-via support
  , AsEnum(..)
  , AsStruct(..)
  , KnownImpliesMax(..)
  ) where

import Codec.Borsh.Class
    ( BorshSizeSum(..)
    , AsEnum(..)
    , AsStruct(..)
    , FromBorsh(..)
    , ToBorsh(..)
    , BorshSize(..)
    , Size(..)
    , KnownSize(..)
    , BorshMaxSize(..)
    , KnownImpliesMax(..)
    , serialiseBorsh
    , deserialiseBorsh
    )
import Codec.Borsh.Encoding (Encoder(..))
import Codec.Borsh.Incremental (Decoder)
import Codec.Borsh.Incremental.Monad (DeserialiseFailure(..))
