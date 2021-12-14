import Data.List ( sort )

unsafeReadInt :: String -> Int
unsafeReadInt s = read s :: Int

parseInput :: IO [Int]
parseInput = fmap (map unsafeReadInt . lines) (readFile "10.in")

pairsDiff :: [Int] -> [Int]
pairsDiff [x] = []
pairsDiff (x:y:ys) = y - x : pairsDiff (y:ys)

fstPart :: IO ()
fstPart = do
    input <- parseInput
    let diffs = pairsDiff $ sort ([0] ++ input ++ [maximum input + 3])
    print $ length (filter (==1) diffs) * length (filter (==3) diffs)