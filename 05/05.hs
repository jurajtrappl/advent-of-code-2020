import Data.List ( sort )
import Data.Bifunctor (Bifunctor(bimap))
import Data.Functor ((<&>))

data Direction
    = U             -- Upper (B, R)
    | B             -- Bottom (F, L)
    deriving Show

data Seat = Seat 
    { row        :: Int
    , col        :: Int
    , identifier :: Int
    } deriving (Eq)

instance Show Seat where
    show x = show (identifier x)

instance Ord Seat where
    x <= y = identifier x <= identifier y

seatingRows :: [Int]
seatingRows = [0..127]

seatingColumns :: [Int]
seatingColumns = [0..7]

binSpacePart :: [Int] -> [Direction] -> Int
binSpacePart [result] [] = result
binSpacePart range (d:ds) = binSpacePart reducedRange ds
    where middleElement = range !! div (length range) 2
          reducedRange = case d of
            B -> [head range..(middleElement - 1)]
            _ -> [middleElement..last range]

selectRow :: [Direction] -> Int
selectRow = binSpacePart seatingRows

selectColumn :: [Direction] -> Int
selectColumn = binSpacePart seatingColumns

seatId :: Int -> Int -> Int
seatId row column = row * 8 + column

parseInput :: IO [([Direction], [Direction])]
parseInput = fmap (map parseDirections . lines) (readFile "05.in")

toDirection :: Char -> Direction
toDirection c = case c of
    'B' -> U
    'R' -> U
    _ -> B

parseDirections :: String -> ([Direction], [Direction])
parseDirections input = bimap (map toDirection) (map toDirection) (rowDirs, colDirs)
    where rowDirs = take 7 input
          colDirs = reverse (take 3 (reverse input))

computePosition :: ([Direction], [Direction]) -> Int
computePosition (rowDirs, colDirs) = seatId (selectRow rowDirs) (selectColumn colDirs)

fstPart :: IO Int
fstPart = parseInput <&> maximum . map computePosition

computeSeat :: ([Direction], [Direction]) -> Seat
computeSeat (rowDirs, colDirs) =
    Seat { row = r, col = c, identifier = seatId r c }
    where r = selectRow rowDirs
          c = selectColumn colDirs

sndPart :: IO ()
sndPart = do
    input <- parseInput
    let allSeats = sort $ map computeSeat input
    let currentSeatIds = map identifier allSeats
    let allSeatIds = [(identifier (head allSeats))..(identifier (last allSeats))]
    print $ sum allSeatIds - sum currentSeatIds

