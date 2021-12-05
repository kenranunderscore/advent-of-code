{-# LANGUAGE LambdaCase #-}

module Day03 where

import Data.List
import Data.Ord

data Bit = Zero | One
  deriving (Ord, Show, Eq)

type Binary = [Bit]

main :: IO ()
main = do
  rows <- lines <$> readFile "./data/day03"
  let vals = readBits <$> rows
      cols = transpose vals
      e = epsilonRate cols
      g = gammaRate cols
  print $ toInt e * toInt g

epsilonRate :: [Binary] -> Binary
epsilonRate = fmap leastCommonBit

gammaRate :: [Binary] -> Binary
gammaRate = fmap flipBit . epsilonRate

toInt :: Binary -> Int
toInt =
  sum
    . map (\(e, x) -> if x == One then 2 ^ e else 0)
    . zip [0 ..]
    . reverse

flipBit :: Bit -> Bit
flipBit = \case
  Zero -> One
  One -> Zero

readBits :: String -> Binary
readBits s = readBit <$> s
  where
    readBit = \case
      '1' -> One
      '0' -> Zero
      x -> error $ show x

leastCommonBit :: (Eq a, Ord a) => [a] -> a
leastCommonBit =
  head
    . concat
    . sortBy (comparing length)
    . group
    . sort
