module Main (main) where

import Test.Tasty

import qualified Test.Codec.Borsh.Roundtrip
import qualified Test.Codec.Borsh.Size

main :: IO ()
main = defaultMain $ testGroup "borsh" [
      Test.Codec.Borsh.Roundtrip.tests
    , Test.Codec.Borsh.Size.tests
    ]
