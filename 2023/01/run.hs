{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.Char

part1 :: IO ()
part1 = do
  input <- readFile "./input"
  let ls = lines input
      ns = fmap
               (\line ->
                   let digits = filter isDigit line
                   in read [head digits, last digits])
               ls
  print $ sum ns

main :: IO ()
main = do
  part1
