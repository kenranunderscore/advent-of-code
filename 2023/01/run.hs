{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import Control.Monad
import Data.Char
import Data.Function ((&))

part1 :: [String] -> Int
part1 ls = do
  let ns = fmap
               (\line ->
                   let digits = filter isDigit line
                   in read @Int [head digits, last digits])
               ls
  sum ns

-- | HAHAHA this is the ugliest solution ever
transform :: String -> String
transform = \case
  [] -> []
  s@(x:xs) ->
    if
      | "one" `isPrefixOf` s -> "1" <> transform (drop (length "one" - 1) s)
      | "two" `isPrefixOf` s -> "2" <> transform (drop (length "two" - 1) s)
      | "three" `isPrefixOf` s -> "3" <> transform (drop (length "three" - 1) s)
      | "four" `isPrefixOf` s -> "4" <> transform (drop (length "four" - 1) s)
      | "five" `isPrefixOf` s -> "5" <> transform (drop (length "five" - 1) s)
      | "six" `isPrefixOf` s -> "6" <> transform (drop (length "six" - 1) s)
      | "seven" `isPrefixOf` s -> "7" <> transform (drop (length "seven" - 1) s)
      | "eight" `isPrefixOf` s -> "8" <> transform (drop (length "eight" - 1) s)
      | "nine" `isPrefixOf` s -> "9" <> transform (drop (length "nine" - 1) s)
      | otherwise -> x : transform xs

part2 :: [String] -> Int
part2 ls = do
  let transformed = fmap transform ls
  part1 transformed

main :: IO ()
main = do
  ls <- lines <$> readFile "input"
  print $ part1 ls
  print $ part2 ls
