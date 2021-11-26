module T02 (fstPart, sndPart) where
    import Util (readInt, splitOn)
    import Data.Bits ( Bits(xor) )
    
    type CompulsoryLetter = Char
    type Letter = Char

    type Password = String

    data Occurrence = Occurrence Int Int
    data Positions = Positions Int Int

    data FstPartPasswd = FstPartPasswd Occurrence CompulsoryLetter Password
    data SndPartPasswd = SndPartPasswd Positions Letter Password

    class Valid a where
        isValid :: a -> Bool

    instance Valid FstPartPasswd where
        isValid (FstPartPasswd (Occurrence lower upper) compulsoryLetter password) =
            lower <= compulsoryLetterCount && upper >= compulsoryLetterCount
            where compulsoryLetterCount = (length . filter (== compulsoryLetter)) password

    instance Valid SndPartPasswd where
        isValid (SndPartPasswd (Positions first second) letter password) =
            xor (password !! (first - 1) == letter) (password !! (second - 1) == letter)
            
    fstPartProcessLine :: [String] -> FstPartPasswd
    fstPartProcessLine [occ, lttr, passwd] = FstPartPasswd occurence (head lttr) passwd
        where [lowerBound, upperBound] = splitOn '-' occ
              occurence = Occurrence (readInt lowerBound) (readInt upperBound)

    sndPartProcessLine :: [String] -> SndPartPasswd
    sndPartProcessLine [pos,lttr,passwd] = SndPartPasswd positions (head lttr) passwd
        where [firstPos, secondPos] = splitOn '-' pos
              positions = Positions (readInt firstPos) (readInt secondPos)

    fstPartProcessInput :: IO [FstPartPasswd]
    fstPartProcessInput = do
        input <- readFile "inputs/02.in"
        let inputLines = map words $ lines input
        return $ map fstPartProcessLine inputLines

    sndPartProcessInput :: IO [SndPartPasswd]
    sndPartProcessInput = do
        input <- readFile "inputs/02.in"
        let inputLines = map words $ lines input
        return $ map sndPartProcessLine inputLines

    fstPart :: IO ()
    fstPart = do
        processedInput <- fstPartProcessInput
        print $ length $ filter isValid processedInput

    sndPart :: IO ()
    sndPart = do
        processedInput <- sndPartProcessInput
        print $ length $ filter isValid processedInput
        
