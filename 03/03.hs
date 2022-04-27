import qualified Data.Matrix as Matrix
import Data.Functor ((<&>))

type Forest = Matrix.Matrix String
type Location = (Int, Int)
type Step = (Int, Int)

treeSign :: String 
treeSign = "#"

countTrees :: Forest -> Step -> Location -> Int -> Integer
countTrees _ _ _ 0 = 0
countTrees _ _ _ (-1) = 0
countTrees forest step@(addRows, addCols) indices@(row, col) counter
    | forest Matrix.! indices == treeSign = 1 + countTrees forest step newIndices newCounter
    | otherwise = countTrees forest step newIndices newCounter
    where newIndices = (row + addRows, col + addCols)
          newCounter = counter - addRows

iter :: (Eq a, Num a) => (b -> b) -> a -> b -> b
iter f 0 x = x
iter f n x = iter f (n-1) (f x)

extendForest :: Forest -> Int -> Forest
extendForest matrix colStep = iter (Matrix.<|> matrix) colFactor matrix
    where minColsFactor = div (Matrix.nrows matrix * colStep) (Matrix.ncols matrix) + 1
          colFactor = if Matrix.ncols matrix >= colStep * Matrix.nrows matrix then 0 else minColsFactor

parseInput :: IO Forest
parseInput = fmap (map (map (:[])) . lines) (readFile "03.in") <&> Matrix.fromLists

fstPart :: IO ()
fstPart = do
    forest <- parseInput
    let scaledForest = extendForest forest 3
    print $ countTrees scaledForest (1, 3) (1, 1) (Matrix.nrows scaledForest)

steps :: [Step]
steps = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]

countTreesWithSteps :: [Step] -> Forest -> Integer
countTreesWithSteps [] _ = 1
countTreesWithSteps (s@(r, c):ss) forest =
    countTrees scaledForest s (1, 1) (Matrix.nrows scaledForest) *
    countTreesWithSteps ss forest
    where scaledForest = extendForest forest c

sndPart :: IO Integer
sndPart = parseInput <&> countTreesWithSteps steps
        