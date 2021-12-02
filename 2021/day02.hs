main :: IO ()
main = do
  rows <- lines <$> readFile "./data/day02"
  let moves = readMove <$> rows
      (finalPos', finalDepth') =
        foldl
          (\(pos, depth) (dir, v) ->
             case dir of
               Up -> (pos, depth - v)
               Forward -> (pos + v, depth)
               Down -> (pos, depth + v)
          )
          (0, 0)
          moves
  print $ finalPos' * finalDepth'
  let (finalPos, finalDepth, finalAim) =
        foldl
          (\(pos, depth, aim) (dir, v) ->
             case dir of
               Down -> (pos, depth, aim + v)
               Up -> (pos, depth, aim - v)
               Forward -> (pos + v, depth + aim * v, aim)
          )
          (0, 0, 0)
          moves
  print $ finalPos * finalDepth

data Direction = Forward | Up | Down
  deriving Show

type Move = (Direction, Int)

readMove :: String -> Move
readMove s =
  let [sdir, sx] = words s
      dir = case sdir of
              "forward" -> Forward
              "up" -> Up
              "down" -> Down
   in (dir, read sx)
