import Data.Functor ((<&>))

preambleLength :: Int
preambleLength = 25

unsafeReadInteger :: String -> Integer
unsafeReadInteger s = read s :: Integer

parseInput :: IO [Integer]
parseInput = fmap (map unsafeReadInteger . lines) (readFile "09.in")

windowsOf :: Int -> [a] -> [[a]]
windowsOf n (x:xs)
    | length xs >= n = (x : take (n - 1) xs) : windowsOf n xs
    | otherwise = [x:xs]

isXamsValid :: [Integer] -> Bool
isXamsValid nums = any (\(a, b) -> a + b == last nums) pairs
    where preamble = take preambleLength nums
          pairs = [(a,b) | (a,b) <- [(x, y) | x <- preamble, y <- preamble], a < b]

fstPart :: IO [Integer]
fstPart = parseInput <&> head . filter (not . isXamsValid) . windowsOf (preambleLength + 1)

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
    invalid <- fstPart
    let contiguousResult = contiguousSumSet input (last invalid)
    print $ minimum contiguousResult + maximum contiguousResult