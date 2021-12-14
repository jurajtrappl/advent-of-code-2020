import Data.Set (toList, fromList)
import qualified Data.Text as Text
import Data.Functor ((<&>))

type Form = [Text.Text]
type Question = Char
type Answer = String

blankLineDelimiter :: Text.Text
blankLineDelimiter = Text.pack "\n\n"

splitGroups :: String -> [Text.Text]
splitGroups = Text.splitOn blankLineDelimiter . Text.pack

parseInput :: IO [Form]
parseInput = fmap (map Text.lines . splitGroups) (readFile "06.in")

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = toList . fromList

fstPart :: IO Int
fstPart = parseInput <&> sum . map (length . removeDuplicates . Text.unpack) . map Text.concat

questions :: [Question]
questions = ['a'..'z']

commonlyAnswered :: [Question] -> [Answer] -> Int
commonlyAnswered [] _ = 0
commonlyAnswered (q:qs) answers
    | all (q `elem`) answers = 1 + commonlyAnswered qs answers
    | otherwise = commonlyAnswered qs answers

sndPart :: IO Int
sndPart = parseInput <&> sum . map (commonlyAnswered questions . map Text.unpack)
