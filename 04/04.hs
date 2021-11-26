{-# LANGUAGE RecordWildCards #-}

module T04 where
    import qualified Data.List as L
    import Data.Maybe ( isJust )
    import qualified Data.Text as T
    import qualified Util as U
    
    fieldsWithoutCid :: [T.Text]
    fieldsWithoutCid = map T.pack [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]

    allValidFields :: [T.Text]
    allValidFields = fieldsWithoutCid ++ [T.pack "cid"]

    validCombinations :: [[T.Text]]
    validCombinations = L.permutations allValidFields ++ L.permutations fieldsWithoutCid

    blankLineDelimiter :: T.Text
    blankLineDelimiter = T.pack "\n\n"

    splitPassportData :: String -> [T.Text]
    splitPassportData = T.splitOn blankLineDelimiter . T.pack

    parseInput :: IO [[T.Text]]
    parseInput = do
        input <- readFile "inputs/04.in"
        let unprocPassports = splitPassportData input
        let withoutNewlines = map T.lines unprocPassports
        return $ map (concatMap T.words) withoutNewlines

    stripData :: T.Text -> T.Text
    stripData = head . T.splitOn (T.pack ":")

    fstPart :: IO ()
    fstPart = do
        input <- parseInput
        let onlyFieldsHeaders = map (map stripData) input
        print $ (length . filter (`elem` validCombinations)) onlyFieldsHeaders

    segmendField :: T.Text -> (T.Text, T.Text)
    segmendField = U.tuplify . T.splitOn (T.pack ":")

    isFieldDataValid :: (T.Text, T.Text) -> Bool
    isFieldDataValid (h, d) = case T.unpack h of
        "byr" -> isValidBirthYear d
        "iyr" -> isValidIssueYear d
        "eyr" -> isValidExpirationYear d
        "hgt" -> isValidHeight d
        "hcl" -> isValidHairColor d
        "ecl" -> isValidEyeColor d
        "pid" -> isValidPassportId d
        _ -> True   --cid

    isPassportValid :: [(T.Text, T.Text)] -> Bool
    isPassportValid = foldr ((&&) . isFieldDataValid) True

    isValidBirthYear :: T.Text -> Bool
    isValidBirthYear value = case maybeBirthYear of
        Just n  -> n >= 1920 && n <= 2002
        Nothing -> False
        where maybeBirthYear = U.readMaybe value :: Maybe Int

    isValidIssueYear :: T.Text -> Bool
    isValidIssueYear value = case maybeIssueYear of
        Just n  -> n >= 2010 && n <= 2020
        Nothing -> False
        where maybeIssueYear = U.readMaybe value :: Maybe Int

    isValidExpirationYear :: T.Text -> Bool
    isValidExpirationYear value = case maybeExpirationYear of
        Just n  -> n >= 2020 && n <= 2030
        Nothing -> False
        where maybeExpirationYear = U.readMaybe value :: Maybe Int

    isValidMetric :: T.Text -> Bool
    isValidMetric value = last splitted == T.empty && isJust maybeUnit && U.fromJust maybeUnit >= 150 && U.fromJust maybeUnit <= 193
        where splitted = T.splitOn (T.pack "cm") value
              maybeUnit = U.readMaybe (head splitted) :: Maybe Int

    isValidImperial :: T.Text -> Bool
    isValidImperial value = last splitted == T.empty && isJust maybeUnit && U.fromJust maybeUnit >= 59 && U.fromJust maybeUnit <= 76
        where splitted = T.splitOn (T.pack "in") value
              maybeUnit = U.readMaybe (head splitted) :: Maybe Int

    isValidHeight :: T.Text -> Bool
    isValidHeight value = isValidMetric value || isValidImperial value
    
    hexaNums :: [Char]
    hexaNums = ['0'..'9'] ++ ['a'..'f']

    isHexa :: T.Text -> Bool
    isHexa value = originalLength == T.length filteredHexas
        where originalLength = T.length value
              filteredHexas = T.filter (`elem` hexaNums) value

    isValidHairColor :: T.Text -> Bool
    isValidHairColor value = T.head value == '#' && T.length num == 6 && isHexa num
        where num = T.drop 1 value

    validEyeColors :: [T.Text]
    validEyeColors = map T.pack [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]

    isValidEyeColor :: T.Text -> Bool
    isValidEyeColor value = value `elem` validEyeColors

    isValidPassportId :: T.Text -> Bool
    isValidPassportId value = T.length value == 9 && case maybePassportId of
        Just n  -> n >= 0 && n <= 999999999
        Nothing -> False
        where maybePassportId = U.readMaybe value :: Maybe Int

    sndPart :: IO Int
    sndPart = do
        input <- parseInput
        let segmented = map (map segmendField) input
        let valid = filter (\ f -> map fst f `elem` validCombinations) segmented
        return $ length $ filter isPassportValid valid