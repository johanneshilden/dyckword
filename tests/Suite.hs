{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid    ( (<>) )
import Control.Monad  ( forM_ )
import Test.Hspec
import System.Exit    ( exitFailure )

import Math.DyckWord.Binary
import Math.DyckWord.Binary.Internal

import qualified Data.Text as T

showN n
  | n > 10^30 = "(huge number)"
  | otherwise = show n

rankAfterUnrank n = do
    putStrLn (showN n)
    rank (unrank n) `shouldBe` n

--checkSize s n = do
--    it ("has the correct size for " ++ show (s, n)) $
--      size (unrankRelative' s n) `shouldBe` s

main :: IO ()
main = hspec $ do

--    describe "rank" $ do
--      forM_ [catalan 431..(catalan 431)+15] rankAfterUnrank 
--      forM_ [catalan 131..(catalan 131)+12] rankAfterUnrank 
--      forM_ [catalan 31..(catalan 31)+8]    rankAfterUnrank 
--      forM_ [catalan 21..(catalan 21)+8]    rankAfterUnrank 
--      forM_ [catalan 11..(catalan 11)+8]    rankAfterUnrank 
--      forM_ [0..1000] rankAfterUnrank     
--      rankAfterUnrank 0
--
--    describe "unrankRelative" $ do
--      checkSize 0 0
--      forM_ [1..12] (checkSize 10)
--      forM_ [1..22] (checkSize 12)
--      forM_ [1..12] (checkSize 22)
--      forM_ [1..22] (checkSize 123)
--      forM_ [1..12] (checkSize 425)
--      forM_ [1..22] (checkSize 1028)

    describe "rank" $ do
      it "is the inverse of unrank" $ 
        forM_ ( [catalan 431..catalan 431+15] 
             ++ [catalan 131..catalan 131+12]
             ++ [catalan  31..catalan 31+8]   
             ++ [catalan  21..catalan 21+8]   
             ++ [catalan  11..catalan 11+8]   
             ++ [0..1000] 
             ) rankAfterUnrank

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


--    return ()

--testWordsOfSize :: SpecWith ()

--    return ()

--testWordsOfSize :: SpecWith ()
--testWordsOfSize =
--    describe "wordsOfSize" $ do
--      it "\nreturns a list of the same length as the corresponding Catalan number" $ do
--        forM_ ([0..10] ++ [12, 14, 15]) $ 
--          \n -> do
--            putStr $ (show n) ++ " "
--            length (wordsOfSize n) `shouldBe` (catalan n)
--
--testConcat :: SpecWith ()
--testConcat = 
--    describe "concatWords" $ do
--      check "()()()" "(())" "()()()(())"
--      check "" "(())()(())" "(())()(())"
--      check "(())()((()))" "" "(())()((()))"
--      check "(())" "()()()" "(())()()()" 
--      check "()" "()" "()()" 
--  where
--    s "" = "\0949"
--    s t  = t
--    check a b c = do
--        it (T.unpack ("returns " <> s c <> " for " <> s a <> " <> " <> s b)) $
--          sum `shouldBe` word
--        it "" $
--          (size sum) `shouldBe` (size word)
--      where
--        sum  = (fromText' a) <> (fromText' b)
--        word = fromText' c
--
--testRank :: SpecWith ()
--testRank = 
--    describe "rank" $ do
--      it "..." $ 
--        rank (unrank 10) `shouldBe` 10
--
--main :: IO ()
--main = hspec $ do
--    testWordsOfSize
--    testConcat
--    testRank
