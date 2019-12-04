main :: IO ()

main = print $ length [x | x <- [146810..612564], isValid x]

breakNum :: Int -> [Int]
breakNum 0 = []
breakNum num = breakNum (num `div` 10) ++ [num `mod` 10]

hasAdjacent :: [Int] -> Bool
hasAdjacent [] = False
hasAdjacent [_] = False
hasAdjacent (x:[y]) | x == y = True | otherwise = False
hasAdjacent (x:xs)
  | x == head xs && x /= (head . tail) xs = True || hasAdjacent (tail xs)
  | x == head xs && x == (head . tail) xs = False || hasAdjacent (firstDiff x (tail xs))
  | otherwise = False || hasAdjacent xs

firstDiff :: Int -> [Int] -> [Int]
firstDiff _ [] = []
firstDiff _ [_] = []
firstDiff a l@(x:xs) | a == x && a /= head xs = xs
                     | a /= x = l
                     | otherwise = firstDiff a xs

neverDecrease :: [Int] -> Bool
neverDecrease [] = True
neverDecrease [_] = True
neverDecrease (x:xs) =  current && neverDecrease xs
  where current = head xs >= x

isValid :: Int -> Bool
isValid num = hasAdjacent brokenNum && neverDecrease brokenNum
  where brokenNum = breakNum num
