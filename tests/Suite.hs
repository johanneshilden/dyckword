{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad ( forM_ )
import Data.Monoid   ( (<>) )
import System.Console.ANSI
import System.Exit   ( exitFailure )
import Test.Hspec

import Math.DyckWord.Binary
import Math.DyckWord.Binary.Internal

import qualified Data.Text as T

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

showN :: Integer -> String
showN n
  | n > 10^30 = "..." ++ lastN 20 (show n) ++ " (huge number)"
  | otherwise = show n

rankAfterUnrank :: Integer -> IO ()
rankAfterUnrank n = do
    setCursorColumn 2
    putStr (showN n)
    clearFromCursorToLineEnd
    rank (unrank n) `shouldBe` n

checkSize :: Integer -> Integer -> IO ()
checkSize s n = do
    setCursorColumn 2
    putStr $ show (s, n)
    clearFromCursorToLineEnd
    size (unrankRelative' s n) `shouldBe` s

makeSure :: Bool -> Expectation
makeSure = shouldBe True  

main :: IO ()
main = hspec $ do

    describe "unrankRelative" $ do
      it "returns a dyck word with the expected size" $ do
        forM_ [10, 12, 22, 123, 425, 1028] $
          forM_ [1..22] . checkSize 
        setCursorColumn 0

    describe "rank" $ do
      it "is the inverse of unrank" $ do
        forM_ ( [catalan  31..catalan 31+80]
             ++ [catalan 131..catalan 131+192]
             ++ [catalan 431..catalan 431+95]
             ++ [catalan  21..catalan 21+284]   
             ++ [catalan  11..catalan 11+989]   
             ++ [0..1000] ) rankAfterUnrank
        setCursorColumn 0

    describe "unrankRelative" $ do
      it "returns Nothing for invalid relative rank" $ do
        unrankRelative 0 2 `shouldBe` Nothing
        unrankRelative 1 8 `shouldBe` Nothing
        unrankRelative 3 5 `shouldBe` Nothing
        unrankRelative 1111 (catalan 1111) `shouldBe` Nothing
        unrankRelative 0 (-1) `shouldBe` Nothing
        unrankRelative 5 (-1) `shouldBe` Nothing
        unrankRelative 5 1231231238837431321283123981237123 `shouldBe` Nothing

    describe "empty" $ do
      it "has size 0" $
        size empty `shouldBe` 0
      it "matches the empty Text value" $
        toText empty `shouldBe` T.empty
      it "has rank 0, and relative rank 0" $ do
        rank empty `shouldBe` 0
        rankRelative empty `shouldBe` 0

    describe "Ord" $ do
      it "works as expected" $ do
        makeSure (unrank 100 <= unrank 101) 
        makeSure (unrank 0 <= unrank 1) 
        makeSure (unrank 1 > unrank 0) 
        makeSure (unrank (10^16) > unrank (10^15)) 
        makeSure $ not (unrank 1235 > unrank 2000) 

    describe "Eq" $ do
      it "works with unrank" $ do
        makeSure (unrank 0 == empty)
        makeSure (unrank 1 == fromText' "()") 
        makeSure (unrank 2 == fromText' "(())") 
        makeSure (unrank 3 == fromText' "()()")
        makeSure (unrank 123 == fromText' "(()(())()())") 
        makeSure (unrank 123 == fromText' "001001101011") 

      it "works with different alphabets" $ do
        makeSure (fromText' "(()(())()())" == fromText' "001001101011") 

