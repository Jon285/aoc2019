main :: IO ()

main = print $ length [x | x <- [146810..612564], isValid x]

breakNum :: Int -> [Int]
breakNum 0 = []
breakNum num = breakNum (num `div` 10) ++ [num `mod` 10]

hasAdjacent :: [Int] -> Bool
hasAdjacent [] = False
hasAdjacent [_] = False
hasAdjacent (x:xs) = current || hasAdjacent xs
  where current = x == head xs

neverDecrease :: [Int] -> Bool
neverDecrease [] = True
neverDecrease [_] = True
neverDecrease (x:xs) =  current && neverDecrease xs
  where current = head xs >= x

isValid :: Int -> Bool
isValid num = hasAdjacent brokenNum && neverDecrease brokenNum
  where brokenNum = breakNum num
