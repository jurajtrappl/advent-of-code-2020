import Data.List ( sort )
import Data.Functor ((<&>))

type JoltageRating = Int
type Sequence = [Int]

unsafeReadInt :: String -> Int
unsafeReadInt s = read s :: Int

parseInput :: IO [JoltageRating]
parseInput = fmap (map unsafeReadInt . lines) (readFile "10.in")

pairsDiff :: [Int] -> [Int]
pairsDiff [x] = []
pairsDiff (x:y:ys) = y - x : pairsDiff (y:ys)

fstPart :: IO ()
fstPart = do
    input <- parseInput
    let diffs = pairsDiff $ sort ([0] ++ input ++ [maximum input + 3])
    print $ length (filter (==1) diffs) * length (filter (==3) diffs)

extendSequence :: [JoltageRating] -> Sequence -> [Sequence]
extendSequence ratings seq = if null appropriateRatings then [seq] else map (\r -> seq ++ [r]) appropriateRatings
    where lastSeqElem = last seq
          appropriateRatings = filter (\r -> r - lastSeqElem > 0 && r - lastSeqElem <= 3) ratings

findSequences :: [Sequence] -> [JoltageRating] -> [Sequence]
findSequences sequences ratings
    | sequences /= newSequences = findSequences newSequences ratings
    | otherwise = sequences
    where newSequences = concatMap (extendSequence ratings) sequences

sndPart :: IO ()
sndPart = do 
    ratings <- parseInput
    print $ length $ findSequences [[0]] (sort ratings ++ [maximum ratings + 3])