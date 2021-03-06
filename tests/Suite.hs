{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad ( forM_ )
import Data.Monoid   ( (<>) )
import System.Console.ANSI
import System.Exit   ( exitFailure )
import System.IO
import Test.Hspec

import Math.DyckWord.Binary
import Math.DyckWord.Binary.Internal

import qualified Data.Text as T

isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft (Right _) = False

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

showN :: Integer -> String
showN n
  | n > 10^30 = "..." ++ lastN 20 (show n) ++ " (huge number)"
  | otherwise = show n

saysThat :: String -> IO a -> IO a
saysThat s action = do
    setCursorColumn 2
    putStr s
    clearFromCursorToLineEnd
    hFlush stdout
    action

resetCursor :: IO ()
resetCursor = do
    setCursorColumn 0
    clearFromCursorToLineEnd

infixl 0 `saysThat` 

makeSure :: Bool -> Expectation
makeSure = shouldBe True  

rankAfterUnrank :: Integer -> IO ()
rankAfterUnrank n = showN n `saysThat` rank (unrank n) `shouldBe` n

checkSize :: Integer -> Integer -> IO ()
checkSize s n = show (s, n) `saysThat` size (unrankRelative' s n) `shouldBe` s

sizeShouldBeHomomorphic :: T.Text -> T.Text -> Expectation
sizeShouldBeHomomorphic a b = size (u <> v) `shouldBe` (size u + size v)
  where
    u = fromText' a
    v = fromText' b

checkIdentity :: Integer -> IO ()
checkIdentity n = show n `saysThat` do 
    empty <> unrank n `shouldBe` (unrank n)
    unrank n <> empty `shouldBe` (unrank n)

checkAssociativity :: (Integer, Integer, Integer) -> IO ()
checkAssociativity triple@(a, b, c) = show triple `saysThat` p
  where
    p = ((u <> v) <> w) `shouldBe` (u <> (v <> w))
    u = unrank a
    v = unrank b
    w = unrank c

checkWordsOfSizeCatalan :: Integer -> IO ()
checkWordsOfSizeCatalan n = 
    show c_n ++ " words of size " ++ show n `saysThat` 
      length (wordsOfSize n) `shouldBe` c_n
  where
    c_n = catalan n

checkWordsOfSizeAgainstUnrank :: Integer -> Integer -> IO ()
checkWordsOfSizeAgainstUnrank i j =
    show (i, j) `saysThat` 
     wordsOfSize i !! (fromIntegral j) `shouldBe` unrankRelative' i j

main :: IO ()
main = hspec $ do

    describe "unrankRelative" $ do
      it "returns a dyck word with the expected size" $ do
        forM_ [10, 12, 22, 123, 425, 1028] $
          forM_ [1..22] . checkSize 
        resetCursor

    describe "rank" $ do
      it "is the inverse of unrank" $ do
        forM_ ( [catalan  31..catalan 31+80]
             ++ [catalan 131..catalan 131+192]
             ++ [catalan 431..catalan 431+95]
             ++ [catalan  21..catalan 21+284]   
             ++ [catalan  11..catalan 11+989]   
             ++ [0..1000] ) rankAfterUnrank
        resetCursor

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

    describe "setAlphabet" $ do
      it "replaces the characters in a word's Text value" $ do
        toText (setAlphabet '0' '1' $ unrank 10) `shouldBe` "00010111"
        toText (setAlphabet '(' ')' $ fromText' "0000111101") `shouldBe` "(((())))()"
        toText (setAlphabet '1' '0' $ fromText' "0000111101") `shouldBe` "1111000010"
        toText (setAlphabet 'a' 'b' $ fromText' "xyxyxxyy") `shouldBe` "ababaabb"

    describe "concatWords" $ do
      it "returns a word with size equal to the sum of the sizes of the inputs" $ do
        sizeShouldBeHomomorphic "(()())" "(((())))"
        sizeShouldBeHomomorphic "ababaaaabbbb" "(((())))"
        sizeShouldBeHomomorphic "" "(((())))"

      it "works with different alphabets" $ do
        concatWords (fromText' "()()") (fromText' "aaaabbbb") `shouldBe` (fromText' "()()(((())))")
        concatWords (fromText' "()()") (fromText' "aaaabbbb") `shouldBe` (fromText' "010100001111")

      it "satisfies the identity law" $ do
        forM_ [0, 20..33350] checkIdentity
        resetCursor

      it "is associative" $ do
        forM_ [(a, b, c) | a <- [0, 20, 1000], b <- [0..300], c <- [0, 50..200]] checkAssociativity
        resetCursor

    describe "wordsOfSize" $ do
      it "matches the Catalan numbers" $ do
        forM_ [0..17] checkWordsOfSizeCatalan
        resetCursor

      it "matches the result of unrankRelative'" $ do
        forM_ [8..12] $ forM_ [8..1125] . checkWordsOfSizeAgainstUnrank
        resetCursor

    describe "fromText" $ do
      it "should return a Left value on bad input" $ do
        fromText "(((" `shouldSatisfy` isLeft
        fromText "((()" `shouldSatisfy` isLeft
        fromText "((())" `shouldSatisfy` isLeft
        fromText "))(" `shouldSatisfy` isLeft
        fromText "))()" `shouldSatisfy` isLeft
        fromText "(()))" `shouldSatisfy` isLeft
        fromText "((x)" `shouldSatisfy` isLeft

