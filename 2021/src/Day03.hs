{-# LANGUAGE LambdaCase #-}

module Day03 where

import Data.Ord
import Data.List

main :: IO ()
main = do
  rows <- lines <$> readFile "./data/day03"
  let vals = readVals <$> rows
      cols = transpose vals
      epsilonRate = leastCommonVal <$> cols
      gammaRate = flipVal <$> epsilonRate
  print gammaRate
  print epsilonRate
  print $ toInt gammaRate
  print $ toInt epsilonRate
  print $ toInt epsilonRate * toInt gammaRate

data Val = Zero | One
  deriving (Ord, Show, Eq)

toInt :: [Val] -> Int
toInt =
  sum
    . map (\(e, x) -> if x == One then 2 ^ e else 0)
    . zip [0..]
    . reverse
  
flipVal :: Val -> Val
flipVal = \case
  Zero -> One
  One -> Zero

readVals :: String -> [Val]
readVals s = 
  readVal <$> s
  where
    readVal = \case
      '1' -> One
      '0' -> Zero
      x -> error $ show x

leastCommonVal :: (Eq a, Ord a) => [a] -> a
leastCommonVal =
  head . concat . sortBy (comparing length) . group . sort
