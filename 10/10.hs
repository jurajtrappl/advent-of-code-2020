import Data.List ( sort )

parseInput :: IO [Int]
parseInput = do
    input <- readFile "10.in"
    return $ map (\s -> read s :: Int) $ lines input

pairsDiff :: [Int] -> [Int]
pairsDiff [x] = []
pairsDiff (x:y:ys) = y - x : pairsDiff (y:ys)

fstPart :: IO ()
fstPart = do
    input <- parseInput
    let diffs = pairsDiff $ sort ([0] ++ input ++ [maximum input + 3])
    print $ length (filter (==1) diffs) * length (filter (==3) diffs)