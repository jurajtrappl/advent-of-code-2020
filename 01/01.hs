module T01 where
    import Util (readInt, uncurry3)
    
    readInputToNumbers :: IO [Int]
    readInputToNumbers = do
        input <- readFile "01.in"
        return $ map readInt $ lines input

    fstPart :: IO ()
    fstPart = do
        numbers <- readInputToNumbers
        let appropriatePairs = [(a, b) | a <- numbers, b <- numbers, a /= b, a + b == 2020]
        print $ uncurry (*) $ head appropriatePairs

    sndPart :: IO ()
    sndPart = do
        numbers <- readInputToNumbers
        let appropriateTriplets = [(a, b, c) | a <- numbers, b <- numbers, a /= b, c <- numbers, b /= c, a + b + c == 2020]
        print $ uncurry3 (*) $ head appropriateTriplets
