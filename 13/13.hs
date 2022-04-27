import Data.List.Split ( splitOn )
import Data.List (minimumBy)
import Data.Function (on)

type BusId = Int
type TimeStamp = Int

unsafeReadInt :: String -> Int
unsafeReadInt value = read value :: Int

parseInput :: IO (TimeStamp, [BusId])
parseInput = do
    fContent <- readFile "13.in"
    let input = lines fContent
    return (read (head input) :: TimeStamp,
            map unsafeReadInt $ filter (/= "x") $ splitOn "," (input !! 1))

formatResult :: (Int, Int) -> Int
formatResult (x, y) = x * (x - y)

fstPart :: IO ()
fstPart = do
    input <- parseInput
    let earliestTimeSt = fst input
    let idTimeDiffTuples = map (\ bid -> (bid, mod earliestTimeSt bid)) $ snd input
    print $ formatResult $ minimumBy (compare `on` uncurry (-)) idTimeDiffTuples