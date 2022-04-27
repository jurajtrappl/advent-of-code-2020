type Expense = Int
type ExpenseReport = [Expense]

unsafeReadInt :: String -> Int
unsafeReadInt s = read s :: Int

readInputToNumbers :: IO ExpenseReport
readInputToNumbers = map unsafeReadInt . lines <$> readFile "01.in"

fstPart :: IO ()
fstPart = do
    numbers <- readInputToNumbers
    let appropriatePairs = [(a, b) | a <- numbers, b <- numbers, a /= b, a + b == 2020]
    print $ uncurry (*) $ head appropriatePairs

uncurry3 :: (a -> b -> a) -> (a, b, b) -> a
uncurry3 f (x, y, z) = f (f x y) z

sndPart :: IO ()
sndPart = do
    numbers <- readInputToNumbers
    let appropriateTriplets = [(a, b, c) | a <- numbers, b <- numbers, a /= b, c <- numbers, b /= c, a + b + c == 2020]
    print $ uncurry3 (*) $ head appropriateTriplets
