{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE LambdaCase #-}

import Debug.Trace
import Data.List
import Control.Monad
import Data.Char
import Data.Maybe qualified as Maybe
import Data.Function ((&))
import Data.Set qualified as S

data Card = Card
  { winningNumbers :: S.Set Int
  , numbersIHave :: S.Set Int
  }
  deriving Show

splitList :: Eq a => a -> [a] -> [[a]]
splitList separator =
  filter (not . null)
  . (\(currentPart, parts) -> currentPart:parts)
  . foldr
      (\char (currentPart, parts) ->
         if char == separator
         then ([], currentPart:parts)
         else (char:currentPart, parts))
      ([], [])

readCard :: String -> Card
readCard s =
  let (_:numbers:_) = splitList ':' s
      (winning:mine:_) = splitList '|' numbers
  in Card (readNumbers winning) (readNumbers mine)
  where
    readNumbers = S.fromList . fmap read . splitList ' '

part1 :: [String] -> Int
part1 input =
  input
    & fmap readCard
    & fmap (\c ->
              let n = length $ S.intersection c.winningNumbers c.numbersIHave
              in if n == 0 then 0 else 2 ^ (n - 1))
    & sum

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  print $ part1 input
