{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Data.Char
import Data.Function ((&))
import Data.List

part1 :: [String] -> Int
part1 =
    sum
        . fmap
            ( \line ->
                let digits = filter isDigit line
                in read @Int [head digits, last digits]
            )

-- | HAHAHA this is the ugliest solution ever
transform :: String -> String
transform = \case
    [] -> []
    s@(x : xs) ->
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
part2 = part1 . fmap transform

main :: IO ()
main = do
    input <- lines <$> readFile "input"
    print $ part1 input
    print $ part2 input
