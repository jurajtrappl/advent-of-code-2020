import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import Data.Set (toList, fromList)

type Form = [String]
type Question = Char
type Answer = String

splitGroups :: String -> [String]
splitGroups = splitOn "\n\n"

parseInput :: IO [Form]
parseInput = fmap (map lines . splitGroups) (readFile "06.in")

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = toList . fromList

fstPart :: IO Int
fstPart = parseInput <&> sum . map ((length . removeDuplicates) . concat)

questions :: [Question]
questions = ['a'..'z']

commonlyAnswered :: [Question] -> [Answer] -> Int
commonlyAnswered [] _ = 0
commonlyAnswered (q:qs) answers
    | all (q `elem`) answers = 1 + commonlyAnswered qs answers
    | otherwise = commonlyAnswered qs answers

sndPart :: IO Int
sndPart = parseInput <&> sum . map (commonlyAnswered questions)
