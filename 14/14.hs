import Data.List (isPrefixOf, foldl')
import Data.Char (digitToInt)
import qualified Data.Map as M

type Mask = String
type Memory = M.Map Int Int
type Environment = (Memory, Mask)

data Command = Mask String | MemAssign Int Integer deriving (Show)

memRange :: [Int]
memRange = [0..2^36-1]

parseIndice :: String -> Int
parseIndice value = read (tail $ takeWhile (/= ']') $ dropWhile (/= '[') value) :: Int

parseCommand :: String -> Command
parseCommand value
    | isPrefixOf "mem" $ head splitted = MemAssign (parseIndice (head splitted)) (read (last splitted) :: Integer)
    | otherwise = Mask (last splitted)
    where splitted = words value

parseInput :: IO [Command]
parseInput = do
    fContent <- readFile "14.in"
    return $ map parseCommand $ lines fContent

toBinString :: Integer -> String
toBinString 0 = show 0
toBinString 1 = show 1
toBinString num = toBinString (div num 2) ++ show (mod num 2)

decFromBinStr :: String -> Int
decFromBinStr = foldl' (\acc x -> acc * 2 + digitToInt x) 0

applyMask :: String -> String -> String
applyMask [] [] = []
applyMask (n:ns) (m:ms)
    | m == 'X' = n : applyMask ns ms
    | otherwise = m : applyMask ns ms

compute :: Integer -> String -> Int
compute value mask = decFromBinStr $ applyMask (toBinString value) mask

execute :: Environment -> Command -> Environment
execute (mem, mask) cmd = case cmd of
    Mask m -> (mem, m)
    MemAssign index value -> (M.updateAt (\ _ _ -> Just (compute value mask)) index mem, mask)

fstPart = do
    commands <- parseInput
    print (M.fromList (zip [0..8] (replicate (9) 0)), "")

    -- return $ fst (foldl (execute) env commands) M.! 8


