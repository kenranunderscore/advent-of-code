{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

import Control.Applicative
import Control.Concurrent.Async qualified as A
import Data.Attoparsec.Text
import Data.Foldable (foldl')
import Data.Functor
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Debug.Trace

data Instruction = L | R
    deriving stock (Show, Eq)

type Instructions = [Instruction]

instructions :: Parser [Instruction]
instructions = many1' instr
  where
    instr = (char 'L' $> L) <|> (char 'R' $> R)

newtype Node = Node {value :: Text}
    deriving stock (Show, Ord, Eq)

type Network = Map Node (Node, Node)

network :: Parser Network
network = M.fromList <$> many1' row
  where
    node = Node . T.pack <$> count 3 anyChar
    row = do
        n <- node
        string " = ("
        l <- node
        string ", "
        r <- node
        string ")\n"
        pure (n, (l, r))

instructionsAndNetwork :: Parser (Instructions, Network)
instructionsAndNetwork = do
    instr <- instructions
    string "\n\n"
    net <- network
    pure (instr, net)

advance :: Network -> Instruction -> Node -> Node
advance net instr node =
    let (l, r) = net M.! node
    in if instr == L then l else r

part1 :: Instructions -> Network -> Int
part1 instrs net =
    go (1, aaa) (cycle instrs)
  where
    go (steps, node) (instr : rest) =
        let next = advance net instr node
        in if next == zzz then steps else go (steps + 1, next) rest
    aaa = Node "AAA"
    zzz = Node "ZZZ"

endsWith :: Char -> Node -> Bool
endsWith c node = last (T.unpack node.value) == c

main :: IO ()
main = do
    input <- T.readFile "input"
    let Right (i, n) = parseOnly instructionsAndNetwork input
    print $ part1 i n

-- print $ part2 i n
