# Revision history for borsh

## 0.3.0 -- 2023-03-17

* Introduce `BorshMaxSize`
* Rename `Struct` (for deriving-via) to `AsStruct`
* Introduce `AsEnum` (for deriving-via) alongside `AsStruct`
  (this is also the reason for the renaming: `Enum` is in the prelude.)
* Remove the generic defaults for `ToBorsh`, `FromBorsh`, `BorshSize` and
  `MaxBorshSize`; users should instead use `AsEnum` or `AsStruct` (#10).

## 0.2.0 -- 2022-11-15

* Remove dependency on `memory`
* Don't newtype-wrap `Int128` and `Word128`
* Improve documentation of the incremental interface
* Make size argument to `FixedSizeArray` nominal

## 0.1.0 -- 2022-11-11

* First released version
