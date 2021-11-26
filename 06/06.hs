module T06 where
    import Data.Set (toList, fromList)
    import qualified Data.Text as T

    blankLineDelimiter :: T.Text
    blankLineDelimiter = T.pack "\n\n"

    splitGroups :: String -> [T.Text]
    splitGroups = T.splitOn blankLineDelimiter . T.pack

    parseInput :: IO [[T.Text]]
    parseInput = do
        input <- readFile "inputs/06.in"
        let unprocGroups = splitGroups input
        return $ map T.lines unprocGroups

    removeDuplicates :: Ord a => [a] -> [a]
    removeDuplicates = toList . fromList

    fstPart :: IO Int
    fstPart = do
        input <- parseInput
        let processedGroups = map T.concat input
        return $ sum $ map (length . removeDuplicates . T.unpack) processedGroups

    questions :: [Char]
    questions = ['a'..'z']
    
    commonlyAnswered :: [Char] -> [String] -> Int
    commonlyAnswered [] _ = 0
    commonlyAnswered (q:qs) answers
        | all (q `elem`) answers = 1 + commonlyAnswered qs answers
        | otherwise = commonlyAnswered qs answers 

    sndPart :: IO Int
    sndPart = do
        input <- parseInput
        let strings = map (map T.unpack) input
        return $ sum $ map (commonlyAnswered questions) strings
