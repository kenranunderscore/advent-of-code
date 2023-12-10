{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

import Control.Applicative
import Data.Attoparsec.Text
import Data.Foldable (foldl')
import Data.Functor
import Data.Text (Text)
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V

data Tile = TopBottom | LeftRight | BottomLeft | BottomRight | TopLeft | TopRight | Start | Empty

instance Show Tile where
  show = \case
    TopBottom -> "║"
    LeftRight -> "═"
    Start -> "S"
    Empty -> " "
    TopLeft -> "╔"
    TopRight -> "╗"
    BottomLeft -> "╚"
    BottomRight -> "╝"

newtype Area = Area { value :: Vector (Vector Tile) }

instance Show Area where
  show (Area rows) =
    List.intercalate "\n" (V.toList $ fmap showRow rows)
    where
      showRow row = concat $ V.toList $ fmap show row

area :: Parser Area
area = do
    Area . V.fromList <$> many1 row
  where
    tile =
        ("." >> pure Empty)
            <|> ("S" >> pure Start)
            <|> ("|" >> pure TopBottom)
            <|> ("-" >> pure LeftRight)
            <|> ("L" >> pure BottomLeft)
            <|> ("J" >> pure BottomRight)
            <|> ("7" >> pure TopRight)
            <|> ("F" >> pure TopLeft)
    row = V.fromList <$> (many1' tile <* char '\n')

part1 :: Text -> Int
part1 input = undefined

main :: IO ()
main = do
    input <- T.readFile "example"
    let Right x = parseOnly area input
    print x
    print $ part1 input
