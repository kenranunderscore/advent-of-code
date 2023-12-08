{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

import Control.Applicative
import Data.Attoparsec.Text
import Data.Foldable (foldl')
import Data.Functor
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

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

aaa = Node "AAA"
zzz = Node "ZZZ"

part1 :: Text -> Int
part1 input =
    let
        Right (instrs, net) = parseOnly instructionsAndNetwork input
        go (steps, node) (instr : rest) =
            let
                (left, right) = net M.! node
                next = if instr == L then left else right
            in
                if next == zzz then steps else go (steps + 1, next) rest
    in
        go (1, aaa) (cycle instrs)

main :: IO ()
main = do
    input <- T.readFile "input"
    let Right x = parseOnly instructionsAndNetwork input
    print x
    print $ part1 input
