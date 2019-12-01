import System.IO (readFile)

main :: IO ()

main = do
  fileInp <- readFile "input.txt"
  print $ calcAll fileInp

calcAll :: String -> Int
calcAll strings = foldl (+) 0 $ map calcFuel intList
  where intList = parseInp strings

parseInp :: String -> [Int]
parseInp string = map (read :: String -> Int) plines
  where plines = lines string

calcFuel :: Int -> Int
calcFuel x = (x `div` 3) - 2
