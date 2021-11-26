import Data.Matrix ( (!), (<|>), fromLists, Matrix(..) )
import Util ( iter )

treeSign :: String 
treeSign = "#"

countTrees :: Matrix String -> (Int, Int) -> (Int, Int) -> Int -> Integer
countTrees _ _ _ 0 = 0
countTrees _ _ _ (-1) = 0
countTrees forest step@(addRows, addCols) indices@(row, col) counter
    | forest ! indices == treeSign = 1 + countTrees forest step newIndices newCounter
    | otherwise = countTrees forest step newIndices newCounter
    where newIndices = (row + addRows, col + addCols)
          newCounter = counter - addRows

extendForest :: Matrix [Char] -> Int -> Matrix [Char]
extendForest matrix colStep = iter (<|> matrix) colFactor matrix
    where minColsFactor = div (nrows matrix * colStep) (ncols matrix) + 1
          colFactor = if ncols matrix >= colStep * nrows matrix then 0 else minColsFactor

parseInput :: IO (Matrix [Char])
parseInput = do
    input <- readFile "03.in"
    return $ fromLists $ map (map (:[])) $ lines input

fstPart :: IO ()
fstPart = do
    forest <- parseInput
    let scaledForest = extendForest forest 3
    print $ countTrees scaledForest (1, 3) (1, 1) (nrows scaledForest)

steps :: [(Int, Int)]
steps = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]

countTreesWithSteps :: Matrix [Char] -> [(Int, Int)] -> Integer
countTreesWithSteps _ [] = 1
countTreesWithSteps forest (s@(r, c):ss) =
    countTrees scaledForest s (1, 1) (nrows scaledForest) *
    countTreesWithSteps forest ss
    where scaledForest = extendForest forest c

sndPart :: IO ()
sndPart = do
    forest <- parseInput
    print $ countTreesWithSteps forest steps
        