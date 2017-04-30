module Main where

import Test.Hspec
import System.Exit ( exitFailure )

import Math.DyckWord.Binary

testWordsOfSize =
    describe "wordsOfSize" $ do
        it "returns a list of the same lenght as the corresponding Catalan number" $ do
            length (wordsOfSize 2) `shouldBe` (2 :: Int)

main :: IO ()
main = hspec $ do
    testWordsOfSize

