import qualified Data.List as List
import Data.Maybe ( isJust, fromJust )
import qualified Data.Text as Text
import Data.Functor ((<&>))

type Field = Text.Text
type FieldData = (Text.Text, Text.Text)
type Passport = [Text.Text]

fieldsWithoutCid :: [Field]
fieldsWithoutCid = map Text.pack [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]

allValidFields :: [Field]
allValidFields = fieldsWithoutCid ++ [Text.pack "cid"]

validCombinations :: [[Field]]
validCombinations = List.permutations allValidFields ++ List.permutations fieldsWithoutCid

blankLineDelimiter :: Text.Text
blankLineDelimiter = Text.pack "\n\n"

splitPassportData :: String -> [Text.Text]
splitPassportData = Text.splitOn blankLineDelimiter . Text.pack

parseInput :: IO [Passport]
parseInput = fmap (map (concatMap Text.words . Text.lines) . splitPassportData) (readFile "04.in")

stripData :: Text.Text -> Text.Text
stripData = head . Text.splitOn (Text.pack ":")

fstPart :: IO Int
fstPart = parseInput <&> length . filter (`elem` validCombinations) . map (map stripData)

tuplify :: [a] -> (a, a)
tuplify [x, y] = (x, y)

segmendField :: Text.Text -> FieldData
segmendField = tuplify . Text.splitOn (Text.pack ":")

isFieldDataValid :: FieldData -> Bool
isFieldDataValid (h, d) = case Text.unpack h of
    "byr" -> isValidBirthYear d
    "iyr" -> isValidIssueYear d
    "eyr" -> isValidExpirationYear d
    "hgt" -> isValidHeight d
    "hcl" -> isValidHairColor d
    "ecl" -> isValidEyeColor d
    "pid" -> isValidPassportId d
    _ -> True   --cid

isPassportValid :: [FieldData] -> Bool
isPassportValid = foldr ((&&) . isFieldDataValid) True

readMaybe :: Read a => Text.Text -> Maybe a
readMaybe s = case reads (Text.unpack s) of
                    [(val, "")] -> Just val
                    _           -> Nothing

isValidBirthYear :: Field -> Bool
isValidBirthYear value = case maybeBirthYear of
    Just n  -> n >= 1920 && n <= 2002
    Nothing -> False
    where maybeBirthYear = readMaybe value :: Maybe Int

isValidIssueYear :: Field -> Bool
isValidIssueYear value = case maybeIssueYear of
    Just n  -> n >= 2010 && n <= 2020
    Nothing -> False
    where maybeIssueYear = readMaybe value :: Maybe Int

isValidExpirationYear :: Field -> Bool
isValidExpirationYear value = case maybeExpirationYear of
    Just n  -> n >= 2020 && n <= 2030
    Nothing -> False
    where maybeExpirationYear = readMaybe value :: Maybe Int

isValidMetric :: Field -> Bool
isValidMetric value = last splitted == Text.empty && isJust maybeUnit && fromJust maybeUnit >= 150 && fromJust maybeUnit <= 193
    where splitted = Text.splitOn (Text.pack "cm") value
          maybeUnit = readMaybe (head splitted) :: Maybe Int

isValidImperial :: Field -> Bool
isValidImperial value = last splitted == Text.empty && isJust maybeUnit && fromJust maybeUnit >= 59 && fromJust maybeUnit <= 76
    where splitted = Text.splitOn (Text.pack "in") value
          maybeUnit = readMaybe (head splitted) :: Maybe Int

isValidHeight :: Field -> Bool
isValidHeight value = isValidMetric value || isValidImperial value

hexaNums :: [Char]
hexaNums = ['0'..'9'] ++ ['a'..'f']

isHexa :: Field -> Bool
isHexa value = originalLength == Text.length filteredHexas
    where originalLength = Text.length value
          filteredHexas = Text.filter (`elem` hexaNums) value

isValidHairColor :: Field -> Bool
isValidHairColor value = Text.head value == '#' && Text.length num == 6 && isHexa num
    where num = Text.drop 1 value

validEyeColors :: [Text.Text]
validEyeColors = map Text.pack [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]

isValidEyeColor :: Field -> Bool
isValidEyeColor value = value `elem` validEyeColors

isValidPassportId :: Field -> Bool
isValidPassportId value = Text.length value == 9 && case maybePassportId of
    Just n  -> n >= 0 && n <= 999999999
    Nothing -> False
    where maybePassportId = readMaybe value :: Maybe Int

sndPart :: IO Int
sndPart = do
    input <- parseInput
    let segmented = map (map segmendField) input
    let valid = filter (\ f -> map fst f `elem` validCombinations) segmented
    return $ length $ filter isPassportValid valid