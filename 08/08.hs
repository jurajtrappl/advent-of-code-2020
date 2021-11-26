import qualified Data.Set as S

data Command = Jmp Int | Acc Int | Nop Int deriving (Show)

parseCommand :: String -> Command
parseCommand line = case head $ words line of
    "acc" -> Acc (sign * number)
    "jmp" -> Jmp (sign * number)
    _ -> Nop (sign * number)
    where numberPart = words line !! 1
          number = read (tail numberPart) :: Int
          sign = if head numberPart == '+' then 1 else -1

fstPartCodeRunner :: [Command] -> Int -> Int -> [Int] -> Int
fstPartCodeRunner code ic acc executed
    | ic `elem` executed = acc
    | otherwise = case code !! ic of
        Acc x -> fstPartCodeRunner code (ic + 1) (acc + x) (ic : executed)
        Jmp x -> fstPartCodeRunner code (ic + x) acc (ic : executed)
        _ -> fstPartCodeRunner code (ic + 1) acc (ic : executed)

fstPart :: IO ()
fstPart = do
    input <- readFile "08.in"
    let commands = map parseCommand $ lines input
    print $ fstPartCodeRunner commands 0 0 []

sndPartCodeRunner :: Int -> Int -> [Int] -> [Command] -> ([Int], Int)
sndPartCodeRunner ic acc executed code
    | ic `elem` executed = (ic : executed, acc)
    | length code == ic = (executed, acc)
    | otherwise =
        case code !! ic of
            Acc x -> sndPartCodeRunner (ic + 1) (acc + x) (ic : executed) code
            Jmp x -> sndPartCodeRunner (ic + x) acc (ic : executed) code
            _ -> sndPartCodeRunner (ic + 1) acc (ic : executed) code

isCorrupted :: [Int] -> Bool
isCorrupted xs = length xs /= length (S.fromList xs)

isSwitchableInstr :: Command -> Bool
isSwitchableInstr instr = case instr of
    Acc x -> False
    _ -> True

findSwitchableInstr :: [Command] -> [Int]
findSwitchableInstr code = map fst $ filter (\(_, i) -> isSwitchableInstr i) $ zip [0..length code - 1] code

switchInstructions :: [Command] -> Int -> [Command]
switchInstructions code ic = xs ++ [newInstruction] ++ xss
    where (xs,_:xss) = splitAt ic code
          newInstruction = case code !! ic of
            Jmp x -> Nop x
            Nop x -> Jmp x

sndPart :: IO ()
sndPart = do
    input <- readFile "08.in"
    let commands = map parseCommand $ lines input
    let switched = map (switchInstructions commands) $ findSwitchableInstr commands
    print $ snd $ head $ filter (not . isCorrupted . fst) $ map (sndPartCodeRunner 0 0 []) switched