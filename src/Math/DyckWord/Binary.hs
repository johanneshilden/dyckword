module Math.DyckWord.Binary
  ( empty
  , size 
  , concatWords 
  , rank 
  , rankRelative 
  , fromText 
  , fromText'
  , toText
  , unrank 
  , unrankRelative 
  , unrankRelative'
  , wordsOfSize 
  ) where

import Data.Maybe                          ( fromJust )
import Data.Monoid                         ( (<>) )
import Data.Text                           ( Text, unfoldrN )
import Math.Combinatorics.Exact.Binomial

import qualified Data.Text                 as T

type Size = Integer
type Rank = Integer

data DyckWord = DyckWord 
  { _size    :: !Size
  , _absRank :: !Rank
  , _relRank :: !Rank
  , _text    :: !Text
  } deriving (Show)

instance Eq DyckWord where
  a == b = _absRank a == _absRank b

instance Ord DyckWord where
  a <= b = _absRank a <= _absRank b

empty :: DyckWord
empty = DyckWord 
  { _size    = 0
  , _absRank = 0
  , _relRank = 0
  , _text    = T.empty } 

size :: DyckWord -> Size
size = _size

concatWords :: DyckWord -> DyckWord -> DyckWord
concatWords a b = fromText' (_text a <> _text b)

instance Monoid DyckWord where
  mappend = concatWords
  mempty = empty

rank :: DyckWord -> Rank
rank = _absRank

rankRelative :: DyckWord -> Rank
rankRelative = _relRank

fromText :: Text -> Either String DyckWord
fromText t 
    | T.null t  = Right empty
    | odd len   = Left "odd length input"
    | a == b    = err
    | otherwise = case T.foldr f (Right (0, s, s)) (T.reverse t) of
        Left e -> Left e
        Right (r,_,_) -> Right $ DyckWord 
          { _size    = s
          , _absRank = catalanSum (fromIntegral s) + r
          , _relRank = r
          , _text    = t } 
  where
    a = T.head t
    b = T.last t
    s = fromIntegral (len `div` 2)
    len = T.length t
    err = Left "bad input"
    f c (Right (i, x, y))
      | x > y || x < 0 = err
      | c == a = Right (i, x - 1, y)
      | c == b = Right (i + catalanTriangle y (x - 1), x, y - 1)
    f _ _ = err

fromText' :: Text -> DyckWord
fromText' t = 
    case fromText t of
      Right r -> r 
      Left  _ -> error "not a valid dyck word"

toText :: DyckWord -> Text
toText = _text

unrank :: Rank -> DyckWord 
unrank r = unrankRelative' s i
  where
    (s, i) = sizeOffs r 0 

sizeOffs :: Integral a => Integer -> a -> (a, Integer)
sizeOffs n x 
    | n < c     = (x, n)
    | otherwise = sizeOffs (n - c) (x + 1) 
  where 
    c = catalan (fromIntegral x)

unrankRelative :: Size -> Rank -> Maybe DyckWord 
unrankRelative s r 
    | r < 0 
   || r >= catalan s = Nothing
    | otherwise    = Just DyckWord 
        { _size    = s
        , _absRank = offset + r
        , _relRank = r
        , _text    = t } 
  where 
    t = unfoldrN (fromIntegral (2*s)) f (r, s, s)
    offset = catalanSum (fromIntegral s)
    f (i, x, y) 
        | 0 == y    = Nothing 
        | j >= 0    = Just (')', (j, x, y - 1))
        | otherwise = Just ('(', (i, x - 1, y)) 
      where 
        j = i - catalanTriangle y (x - 1) 

unrankRelative' :: Size -> Rank -> DyckWord
unrankRelative' s = fromJust . unrankRelative s

wordsOfSize :: Size -> [DyckWord]
wordsOfSize = ofSize unrankRelative' 

ofSize :: (Size -> Integer -> b) -> Size -> [b]
ofSize f s = f s <$> [0 .. catalan (fromIntegral s) - 1]

catalanTriangle :: Integer -> Integer -> Integer
catalanTriangle _ 0 = 1
catalanTriangle n k = (choose (n + k) (k - 1) * (n - k + 1)) `div` k 

catalanSum :: Int -> Integer
catalanSum = (scanl (+) 0 (catalan <$> [0..]) !!)

catalan :: (Integral a, Integral b) => a -> b
catalan 0 = 1
catalan 1 = 1
catalan 2 = 2
catalan n = let m = fromIntegral n in (2*m) `choose` m `div` (m + 1) 
