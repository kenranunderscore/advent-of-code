{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

import Control.Monad
import Data.Char
import Data.Function ((&))
import Data.List
import Data.Maybe qualified as Maybe
import Data.Set qualified as S
import Data.Vector qualified as V
import Debug.Trace

data Card = Card
    { winningNumbers :: S.Set Int
    , numbersIHave :: S.Set Int
    }
    deriving (Show)

splitList :: Eq a => a -> [a] -> [[a]]
splitList separator =
    filter (not . null)
        . (\(currentPart, parts) -> currentPart : parts)
        . foldr
            ( \char (currentPart, parts) ->
                if char == separator
                    then ([], currentPart : parts)
                    else (char : currentPart, parts)
            )
            ([], [])

readCard :: String -> Card
readCard s =
    let
        (_ : numbers : _) = splitList ':' s
        (winning : mine : _) = splitList '|' numbers
    in
        Card (readNumbers winning) (readNumbers mine)
  where
    readNumbers = S.fromList . fmap read . splitList ' '

matchingNumbers :: Card -> Int
matchingNumbers c =
    length $ S.intersection c.winningNumbers c.numbersIHave

part1 :: [String] -> Int
part1 input =
    input
        & fmap readCard
        & fmap
            ( \card ->
                let n = matchingNumbers card
                in if n == 0 then 0 else 2 ^ (n - 1)
            )
        & sum

type Cards = V.Vector Card

readCards :: [String] -> Cards
readCards = V.fromList . fmap readCard

cardScore :: Cards -> Int -> Int
cardScore allCards cardIndex =
    let n = matchingNumbers (allCards V.! cardIndex)
    in 1 + (sum $ fmap (cardScore allCards) [cardIndex + 1 .. cardIndex + n])

part2 :: [String] -> Int
part2 input =
    allCards
        & V.imap (\i _ -> cardScore allCards i)
        & sum
  where
    allCards = readCards input

main :: IO ()
main = do
    input <- lines <$> readFile "input"
    print $ part1 input
    print $ part2 input
