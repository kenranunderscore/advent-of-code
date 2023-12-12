{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoFieldSelectors #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async qualified as A
import Control.Concurrent.MVar
import Control.Monad
import Data.Attoparsec.Text
import Data.Foldable (foldl')
import Data.Functor
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.Internal qualified as Set
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

zloopFinder :: Instructions -> Network -> Node -> Int
zloopFinder instrs net =
    go instrs 1 Nothing
  where
    go (i : rest) steps zprev curr = do
        let next = advance net i curr
        if endsWith 'Z' next
            then case zprev of
                Nothing ->
                    go rest (steps + 1) (Just (next, steps)) next
                Just (target, n) ->
                    steps - n
            else go rest (steps + 1) zprev next

-- I don't like part 2... this solution only works under specific circumstances
part2 :: Instructions -> Network -> Int
part2 instrs net = do
    let
        startNodes = filter (endsWith 'A') $ M.keys net
        (x : xs) = fmap (zloopFinder instrs net) startNodes
     in
        foldl lcm x xs

main :: IO ()
main = do
    input <- T.readFile "input"
    let Right (i, n) = parseOnly instructionsAndNetwork input
    -- print $ part1 i n
    print $ part2 (cycle i) n
