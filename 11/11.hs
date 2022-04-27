import qualified Data.Matrix as Matrix
import Prelude hiding (floor)
import Data.Functor ((<&>))

type Location = (Int, Int)
type SeatLayout = Matrix.Matrix [Char]

floor :: String
floor = "."

emptySeat :: String
emptySeat = "L"

occupiedSeat :: String
occupiedSeat = "#"

parseInput :: IO SeatLayout
parseInput = fmap (map (map (:[])) . lines) (readFile "11.in") <&> Matrix.fromLists

isSeatEqualTo :: String -> SeatLayout -> Location -> Bool
isSeatEqualTo char seats (r, c) = case Matrix.safeGet r c seats of
    Just v -> v == char
    Nothing -> False

isUnoccupied :: SeatLayout -> Location -> Bool
isUnoccupied seats location = not $ isSeatEqualTo occupiedSeat seats location

getNeighbours :: Location -> [Location]
getNeighbours location@(r, c) = filter (/= location) [(a, b) | a <- [r - 1..r + 1], b <- [c - 1..c + 1]]

isEmptySeat :: SeatLayout -> Location -> Bool
isEmptySeat seats location@(r, c) =
    isSeatEqualTo emptySeat seats location && all (isUnoccupied seats) (getNeighbours location)

isOccupiedAdj :: SeatLayout -> Location -> Bool
isOccupiedAdj seats location@(r, c) =
    isSeatEqualTo occupiedSeat seats location && adjacentOccupiedCount >= 4
    where adjacentOccupiedCount = length $ filter (== True) (map (isSeatEqualTo occupiedSeat seats) (getNeighbours location))

updateSeat :: SeatLayout -> SeatLayout -> Location -> SeatLayout
updateSeat seats finalSeats location
    | isEmptySeat seats location = Matrix.setElem occupiedSeat location finalSeats
    | isOccupiedAdj seats location = Matrix.setElem emptySeat location finalSeats
    | otherwise = finalSeats

seatPassengers :: SeatLayout -> SeatLayout -> Location -> SeatLayout
seatPassengers seatsInitial finalSeats location@(r, c)
    | Matrix.nrows seatsInitial == r && Matrix.ncols seatsInitial == c = newSeats
    | Matrix.ncols seatsInitial == c = seatPassengers seatsInitial newSeats (r + 1, 1)
    | otherwise = seatPassengers seatsInitial newSeats (r, c + 1)
    where newSeats = updateSeat seatsInitial finalSeats location

applySeatRules :: [SeatLayout] -> SeatLayout -> Int
applySeatRules computed seats
    | newSeats `elem` computed = countUnoccupied newSeats (1, 1)
    | otherwise = applySeatRules (newSeats:computed) newSeats
    where newSeats = seatPassengers seats seats (1, 1)

countUnoccupied :: SeatLayout -> Location -> Int
countUnoccupied seats location@(r, c)
    | Matrix.nrows seats == r && Matrix.ncols seats == c = add
    | Matrix.ncols seats == c = add + countUnoccupied seats (r + 1, 1)
    | otherwise = add + countUnoccupied seats (r, c + 1)
    where add = if seats Matrix.! location == occupiedSeat then 1 else 0

fstPart :: IO Int
fstPart = parseInput <&> applySeatRules []