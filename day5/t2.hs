import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

data ParamMode = Imm | Pos
  deriving (Eq, Show)

data Opcode = Sum | Mult | JmpT | JmpF | Less | Equal | Output | Input | Halt
  deriving (Eq, Show)

data Instruction = Instruction { operation :: Opcode, params :: Maybe (ParamMode, ParamMode) }
  deriving (Eq, Show)

main :: IO ()
main = do
  fileInput <- readFile "input.txt"
  let parsedInput = (read :: String -> Int) <$> splitOn "," (init fileInput)
  execProgram (0, parsedInput)
  -- print parsedInput
  
changeAt :: Int -> a -> [a] -> [a]
changeAt n a list = (\(l, _:xs) -> l ++ a:xs) $ splitAt n list

parseInstruction :: Int -> Instruction
parseInstruction inst
  | op == 99 = Instruction Halt   Nothing
  | op == 3 =  Instruction Input  Nothing
  | op == 4 =  Instruction Output (Just (firstParam, firstParam ))
  | op == 1 =  Instruction Sum    (Just (firstParam, secondParam))
  | op == 2 =  Instruction Mult   (Just (firstParam, secondParam))
  | op == 5 =  Instruction JmpT   (Just (firstParam, secondParam))
  | op == 6 =  Instruction JmpF   (Just (firstParam, secondParam))
  | op == 7 =  Instruction Less   (Just (firstParam, secondParam))
  | op == 8 =  Instruction Equal  (Just (firstParam, secondParam))
  | otherwise = undefined
  where op = inst `mod` 100
        (firstParam, secondParam) =
          case ((inst `div` 100) `mod` 10, (inst `div` 1000) `mod` 10) of
            (0, 0) -> (Pos, Pos)
            (0, 1) -> (Pos, Imm)
            (1, 0) -> (Imm, Pos)
            (1, 1) -> (Imm, Imm)
            (_, _) -> (Pos, Pos)

execOpCode :: Instruction -> (Int, [Int]) -> IO (Int, [Int])
execOpCode (Instruction Input _) (pointer, source) = do
  input <- getLine
  let parsedInput = (read :: String -> Int) input
  return (pointer + 2, changeAt loc parsedInput source)
    where loc = source !! (pointer + 1)

execOpCode (Instruction Output (Just (param, _))) (pointer, source) = do
  print value
  return (pointer + 2, source)
    where value = if param == Pos
                  then source !! (source !!(pointer + 1))
                  else source !! (pointer + 1)

-- I know, I know, pretty ugly
execOpCode inst (pointer, state) =
  case operation inst of
    Sum   -> return ( pointer + 4, changeAt loc (lhs + rhs) state )
    Mult  -> return ( pointer + 4, changeAt loc (lhs * rhs) state )
    JmpT  -> return ( if lhs /= 0 then rhs else pointer + 3, state )
    JmpF  -> return ( if lhs == 0 then rhs else pointer + 3, state )
    Less  -> return ( pointer + 4, changeAt loc (if lhs < rhs then 1 else 0) state )
    Equal -> return ( pointer + 4, changeAt loc (if lhs == rhs then 1 else 0) state )
    _     -> return ( pointer, state )
  where
    (lhsMode, rhsMode) = fromJust $ params inst
    lhs | lhsMode == Imm = state !! (pointer + 1)
        | otherwise = state !! (state !! (pointer + 1))
    rhs | rhsMode == Imm = state !! (pointer + 2)
        | otherwise = state !! (state !! (pointer + 2))
    loc = state !! (pointer + 3)
    
execProgram :: (Int, [Int]) -> IO ()
execProgram state@(opPos, source)
  | operation opcode == Halt = return ()
  | otherwise = execOpCode opcode state >>= execProgram
  where opcode = parseInstruction (source !! opPos)
