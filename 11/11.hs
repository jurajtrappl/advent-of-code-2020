module T11 where

    import qualified Data.Matrix as M
    import Prelude hiding (floor)
    
    type SeatLayout = M.Matrix [Char]

    floor :: String
    floor = "."

    emptySeat :: String
    emptySeat = "L"

    occupiedSeat :: String
    occupiedSeat = "#"

    parseInput :: IO SeatLayout
    parseInput = do
        input <- readFile "11.in"
        return $ M.fromLists $ map (map (:[])) $ lines input

    isSeatEqualTo :: SeatLayout -> (Int, Int) -> String -> Bool
    isSeatEqualTo seats (r, c) char = case M.safeGet r c seats of
        Just v -> v == char
        Nothing -> False

    isUnoccupied :: SeatLayout -> (Int, Int) -> Bool
    isUnoccupied seats indices = not $ isSeatEqualTo seats indices occupiedSeat

    isEmptySeat :: SeatLayout -> (Int, Int) -> Bool
    isEmptySeat seats indices@(r, c) =
        isSeatEqualTo seats indices emptySeat &&
        isUnoccupied seats (r, c - 1) && -- left
        isUnoccupied seats (r, c + 1) && -- right
        isUnoccupied seats (r - 1, c) && -- up
        isUnoccupied seats (r + 1, c) && -- bottom
        isUnoccupied seats (r - 1, c - 1) && -- diag upper left
        isUnoccupied seats (r - 1, c + 1) && -- diag upper right
        isUnoccupied seats (r + 1, c - 1) && -- diag bottom left
        isUnoccupied seats (r + 1, c + 1) -- diag bottom right

    isOccupiedAdj :: SeatLayout -> (Int, Int) -> Bool
    isOccupiedAdj seats indices@(r, c) =
        isSeatEqualTo seats indices occupiedSeat && adjacentOccupiedCount >= 4
        where adjacentOccupiedCount = length $ filter (== True)
                          [ isSeatEqualTo seats (r, c - 1) occupiedSeat
                          , isSeatEqualTo seats (r, c + 1) occupiedSeat
                          , isSeatEqualTo seats (r - 1, c) occupiedSeat
                          , isSeatEqualTo seats (r + 1, c) occupiedSeat
                          , isSeatEqualTo seats (r - 1, c - 1) occupiedSeat
                          , isSeatEqualTo seats (r - 1, c + 1) occupiedSeat
                          , isSeatEqualTo seats (r + 1, c - 1) occupiedSeat
                          , isSeatEqualTo seats (r + 1, c + 1) occupiedSeat ]

    updateSeat :: SeatLayout -> SeatLayout -> (Int, Int) -> SeatLayout
    updateSeat seats finalSeats indices
        | isEmptySeat seats indices = M.setElem occupiedSeat indices finalSeats
        | isOccupiedAdj seats indices = M.setElem emptySeat indices finalSeats
        | otherwise = finalSeats

    seatPassengers :: SeatLayout -> SeatLayout -> (Int, Int) -> SeatLayout
    seatPassengers seatsInitial finalSeats indices@(r, c)
        | M.nrows seatsInitial == r && M.ncols seatsInitial == c = newSeats
        | M.ncols seatsInitial == c = seatPassengers seatsInitial newSeats (r + 1, 1)
        | otherwise = seatPassengers seatsInitial newSeats (r, c + 1)
        where newSeats = updateSeat seatsInitial finalSeats indices

    applySeatRules :: SeatLayout -> Int
    applySeatRules seats = applySeatRulesHelper seats []

    applySeatRulesHelper :: SeatLayout -> [SeatLayout] -> Int
    applySeatRulesHelper seats computed
        | newSeats `elem` computed = countUnoccupied newSeats (1, 1)
        | otherwise = applySeatRulesHelper newSeats (newSeats:computed)
        where newSeats = seatPassengers seats seats (1, 1)

    countUnoccupied :: SeatLayout -> (Int, Int) -> Int
    countUnoccupied seats indices@(r, c)
        | M.nrows seats == r && M.ncols seats == c = add
        | M.ncols seats == c = add + countUnoccupied seats (r + 1, 1)
        | otherwise = add + countUnoccupied seats (r, c + 1)
        where add = if seats M.! indices == occupiedSeat then 1 else 0

    fstPart :: IO Int
    fstPart = do
        input <- parseInput
        return $ applySeatRules input