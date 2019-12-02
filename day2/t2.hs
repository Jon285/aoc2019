import Data.List.Split (splitOn)

main :: IO ()

main = do
  fileInput <- readFile "input.txt"
  let parsedInput = (read :: String -> Int) <$> splitOn "," (init fileInput)
  print $ problem2 parsedInput
  print $ problem2B parsedInput 100 0

changeAt :: Int -> a -> [a] -> [a]
changeAt n a list = (\(l, _:xs) -> l ++ a:xs) $ splitAt n list

-- Just to help the bruteforce
updateSource :: [Int] -> Int -> Int -> [Int]
updateSource source x y = changeAt 1 x $ changeAt 2 y source

execOpCode :: Int -> Int -> [Int] -> [Int]
execOpCode 1 at source = changeAt loc (lhs + rhs) source
  where lhs = source !! (source !! (at + 1))
        rhs = source !! (source !! (at + 2))
        loc = source !! (at + 3)
        
execOpCode 2 at source = changeAt loc (lhs * rhs) source
  where lhs = source !! (source !! (at + 1))
        rhs = source !! (source !! (at + 2))
        loc = source !! (at + 3)

execProgram :: [Int] -> Int -> [Int]
execProgram source opPos
  | opcode == 99 = source
  | otherwise = execProgram (execOpCode opcode opPos source) nextOp
  where opcode = source !! opPos
        nextOp = opPos + 4

-- Bruteforce Solution
problem2 source = [(x, y) | x <- [1..99], y <- [1..99], head (execProgram (updateSource source x y) 0) == 19690720]

-- Binary Search solution
problem2B source high low
  | 19690720 - result < 100 && 19690720 - result > 0 = (pivot, (19690720 - result))
  | result < 19690720 = problem2B source high pivot
  | otherwise = problem2B source pivot low
  where pivot = ((high - low) `div` 2) + low
        result = (head (execProgram (changeAt 1 pivot source) 0))
