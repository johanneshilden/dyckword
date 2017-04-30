module Main where

import Control.Monad ( forM_ )
import Test.Hspec
import System.Exit ( exitFailure )

import Math.DyckWord.Binary
import Math.DyckWord.Binary.Internal

testWordsOfSize :: SpecWith ()
testWordsOfSize =
    describe "wordsOfSize" $ do
      it "returns a list of the same lenght as the corresponding Catalan number" $ do
        forM_ ([0..10] ++ [12, 14]) $ 
          \n -> length (wordsOfSize n) `shouldBe` (catalan n)

main :: IO ()
main = hspec $ do
    testWordsOfSize

