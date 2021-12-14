import Data.Bits ( Bits(xor) )
import Data.List.Split (splitOn)
import Data.Functor ((<&>))

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

unsafeReadInt :: String -> Int
unsafeReadInt s = read s :: Int

fstPartProcessLine :: [String] -> FstPartPasswd
fstPartProcessLine [occ, lttr, passwd] = FstPartPasswd occurence (head lttr) passwd
    where [lowerBound, upperBound] = splitOn "-" occ
          occurence = Occurrence (unsafeReadInt lowerBound) (unsafeReadInt upperBound)

sndPartProcessLine :: [String] -> SndPartPasswd
sndPartProcessLine [pos,lttr,passwd] = SndPartPasswd positions (head lttr) passwd
    where [firstPos, secondPos] = splitOn "-" pos
          positions = Positions (unsafeReadInt firstPos) (unsafeReadInt secondPos)

solve :: (Functor f, Valid a) => f [a] -> f Int
solve processF = processF <&> length . filter isValid

parseInput :: ([String] -> b) -> IO [b]
parseInput processLineF = map (processLineF . words) . lines <$> readFile "02.in"

fstPart :: IO Int
fstPart = solve $ parseInput fstPartProcessLine

sndPart :: IO Int
sndPart = solve $ parseInput sndPartProcessLine

