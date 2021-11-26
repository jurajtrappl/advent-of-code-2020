module T09 where
    
    import Data.List.Split (chunksOf)

    preambleLength :: Int
    preambleLength = 25

    parseInput :: IO [Integer]
    parseInput = do
        input <- readFile "09.in"
        return $ map (\s -> read s :: Integer) $ lines input

    windowsOf :: Int -> [a] -> [[a]]
    windowsOf n (x:xs)
        | length xs >= n = (x : take (n - 1) xs) : windowsOf n xs
        | otherwise = [x:xs]

    isXamsValid :: [Integer] -> Bool
    isXamsValid nums = any (\(a, b) -> a + b == lastNum) pairs
        where preamble = take preambleLength nums
              pairs = [(a,b) | (a,b) <- [(x, y) | x <- preamble, y <- preamble], a < b]
              lastNum = last nums

    fstPart :: IO ()
    fstPart = do
        input <- parseInput
        let windows = windowsOf (preambleLength + 1) input
        print $ head $ filter (not . isXamsValid) windows

    contiguousSumSet :: [Integer] -> Integer -> [Integer]
    contiguousSumSet list@(x:xs) n
        | null contiguousNums = contiguousSumSet xs n
        | otherwise = contiguousNums
        where contiguousNums = startContiguous list n []

    startContiguous :: [Integer] -> Integer -> [Integer] -> [Integer]
    startContiguous (x:xs) n visited
        | summed < n = startContiguous xs n (x:visited)
        | summed == n = visited
        | otherwise = []
        where summed = sum visited

    sndPart :: IO ()
    sndPart = do
        input <- parseInput
        let windows = windowsOf (preambleLength + 1) input
        let firstInvalid = last $ head $ filter (not . isXamsValid) windows
        let contiguousResult = contiguousSumSet input firstInvalid
        print $ minimum contiguousResult + maximum contiguousResult