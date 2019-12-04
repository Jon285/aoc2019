{- The first and extremely innefective solution I made -}

import Data.List.Split (splitOn)
import Data.List (intersect)

main :: IO ()

newtype Coords = Coords (Int, Int)
  deriving (Show, Eq)

main = do
  input <- readFile "input.txt"
  let wires = split <$> lines input
  let firstw = (parse . head) wires
  let secondw = (parse . second) wires
  print $ closest $ firstw `intersect` secondw
    where closest = minimum . map intersectionDistance
          parse = parseAll (Coords (0, 0))
          split = splitOn ","
          second = head . tail

intersectionDistance :: Coords -> Int
intersectionDistance (Coords (x, y)) = abs x + abs y 

parseAll :: Coords -> [String] -> [Coords]
parseAll _ [] = []
parseAll coord (x:xs) = curr ++ parseAll (last curr) xs
  where curr = parseCoord coord x

parseCoord :: Coords -> String -> [Coords]
parseCoord _ [] = []
parseCoord (Coords (x, y)) (dir:xs)
  | steps == 0 = []
  | dir == 'R' = Coords (x + 1, y) : parseCoord (Coords (x + 1, y)) ('R': show (steps - 1))
  | dir == 'L' = Coords (x - 1, y) : parseCoord (Coords (x - 1, y)) ('L': show (steps - 1))
  | dir == 'U' = Coords (x, y + 1) : parseCoord (Coords (x, y + 1)) ('U': show (steps - 1))
  | dir == 'D' = Coords (x, y + 1) : parseCoord (Coords (x, y - 1)) ('D': show (steps - 1))
  where steps = (read :: String -> Int) xs

-- just to make GHC warnings stop
parseCoord _ _ = []
