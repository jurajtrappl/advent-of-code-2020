import qualified Data.List as List
import Data.Maybe ( isJust, fromJust )
import Data.Functor ((<&>))
import Data.List.Split (splitOn)

type Field = String
type FieldData = (String, String)
type Passport = [String]

fieldsWithoutCid :: [Field]
fieldsWithoutCid = [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]

allValidFields :: [Field]
allValidFields = fieldsWithoutCid ++ ["cid"]

validCombinations :: [[Field]]
validCombinations = List.permutations allValidFields ++ List.permutations fieldsWithoutCid

parseInput :: IO [Passport]
parseInput = fmap (map (concatMap words . lines) . splitOn "\n\n") (readFile "04.in")

stripData :: String -> String
stripData = head . splitOn ":"

fstPart :: IO Int
fstPart = parseInput <&> length . filter (`elem` validCombinations) . map (map stripData)

tuplify :: [a] -> (a, a)
tuplify [x, y] = (x, y)

segmendField :: String -> FieldData
segmendField = tuplify . splitOn ":"

isFieldDataValid :: FieldData -> Bool
isFieldDataValid (h, d) = case h of
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

unsafeReadInt :: String -> Int
unsafeReadInt s = read s :: Int

isValidBirthYear :: Field -> Bool
isValidBirthYear value = let birthYear = unsafeReadInt value
                         in birthYear >= 1920 && birthYear <= 2002

isValidIssueYear :: Field -> Bool
isValidIssueYear value = let issueYear = unsafeReadInt value
                         in issueYear >= 2010 && issueYear <= 2020

isValidExpirationYear :: Field -> Bool
isValidExpirationYear value = let expirationYear = unsafeReadInt value
                              in expirationYear >= 2020 && expirationYear <= 2030

isValidMetric :: Field -> Bool
isValidMetric value = let splitted = splitOn "cm" value
                          unit = unsafeReadInt $ head splitted
                      in last splitted == "" && unit >= 150 && unit <= 193

isValidImperial :: Field -> Bool
isValidImperial value = let splitted = splitOn "in" value
                            unit = unsafeReadInt $ head splitted
                        in last splitted == "" && unit >= 59 && unit <= 76

isValidHeight :: Field -> Bool
isValidHeight value = isValidMetric value || isValidImperial value

hexaNums :: [Char]
hexaNums = ['0'..'9'] ++ ['a'..'f']

isHexa :: Field -> Bool
isHexa value = length value == length (filter (`elem` hexaNums) value)

isValidHairColor :: Field -> Bool
isValidHairColor value = head value == '#' && length (tail value) == 6 && isHexa (tail value)

validEyeColors :: [String]
validEyeColors = [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]

isValidEyeColor :: Field -> Bool
isValidEyeColor value = value `elem` validEyeColors

isValidPassportId :: Field -> Bool
isValidPassportId value = let passportId = unsafeReadInt value
                          in length value == 9 && passportId >= 0 && passportId <= 999999999

sndPart :: IO Int
sndPart = do
    input <- parseInput
    let segmented = map (map segmendField) input
    let valid = filter (\ f -> map fst f `elem` validCombinations) segmented
    return $ length $ filter isPassportValid valid