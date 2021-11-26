import Data.List as L ( sort )

seatingRows :: [Int]
seatingRows = [0..127]

seatingColumns :: [Int]
seatingColumns = [0..7]

-- U -> Upper (B, R) | B -> Bottom (F, L)
data Direction = U | B deriving Show

binSpacePart :: [Direction] -> [Int] -> Int
binSpacePart [] [result] = result
binSpacePart (d:ds) range = binSpacePart ds reducedRange
    where middleElement = range !! div (length range) 2
          reducedRange = case d of
            B -> [head range..(middleElement - 1)]
            _ -> [middleElement..last range]

selectRow :: [Direction] -> Int
selectRow dirs = binSpacePart dirs seatingRows

selectColumn :: [Direction] -> Int
selectColumn dirs = binSpacePart dirs seatingColumns

seatId :: Int -> Int -> Int
seatId row column = row * 8 + column

parseInput :: IO [([Direction], [Direction])]
parseInput = do
    input <- readFile "05.in"
    return $ map parseDirections $ lines input

toDirection :: Char -> Direction
toDirection c = case c of
    'B' -> U
    'R' -> U
    _ -> B

parseDirections :: String -> ([Direction], [Direction])
parseDirections input = (map toDirection rowDirs, map toDirection colDirs)
    where rowDirs = take 7 input
          colDirs = reverse (take 3 (reverse input))

computePosition :: ([Direction], [Direction]) -> Int
computePosition (rowDirs, colDirs) = seatId (selectRow rowDirs) (selectColumn colDirs)

fstPart :: IO ()
fstPart = do
    input <- parseInput
    print $ maximum $ map computePosition input

data Seat = Seat
    { row        :: Int
    , col        :: Int
    , identifier :: Int
    } deriving (Eq)

instance Show Seat where
    show x = show (identifier x)

instance Ord Seat where
    x <= y = identifier x <= identifier y

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

