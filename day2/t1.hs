import Data.List.Split (splitOn)

main :: IO ()

main = do
  fileInput <- readFile "input.txt"
  let parsedInput = (read :: String -> Int) <$> splitOn "," (init fileInput)
  let modInput = changeAt 1 12 $ changeAt 2 2 parsedInput
  print $ execProgram modInput 0

changeAt :: Int -> a -> [a] -> [a]
changeAt n a list = (\(l, _:xs) -> l ++ a:xs) $ splitAt n list

execProgram :: [Int] -> Int -> [Int]
execProgram source opPos
  | opcode == 99 = source
  | otherwise = execProgram (changeAt pos result source) $ nextOp
  where opcode = source !! opPos
        lhs = source !! (source !! (opPos + 1))
        rhs = source !! (source !! (opPos + 2))
        result = if opcode == 1
          then lhs + rhs
          else lhs * rhs
        pos = source !! (opPos + 3)
        nextOp = opPos + 4
