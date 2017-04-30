{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid    ( (<>) )
import Control.Monad  ( forM_ )
import Test.Hspec
import System.Exit    ( exitFailure )

import Math.DyckWord.Binary
import Math.DyckWord.Binary.Internal

import qualified Data.Text as T

main :: IO ()
main = do

    return ()


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
