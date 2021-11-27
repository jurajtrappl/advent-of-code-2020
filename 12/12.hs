{-# OPTIONS_GHC -Wno-missing-methods #-}

import Prelude hiding (Left, Right)

type Degrees = Int

data Direction =
    North Int |
    South Int |
    East Int |
    West Int |
    Left Degrees |
    Right Degrees |
    Forward Int
    deriving (Show)

data Vector = V Int Int

instance Show Vector where
    show (V x y) = "[" ++ show x ++ "," ++ show y ++ "]"

instance Num Vector where
    (V x y) + (V x' y') = V (x + x') (y + y')

type Rotation = Vector

scaleV :: Vector -> Int -> Vector
scaleV (V x y) factor = V (x * factor) (y * factor)

scaleRotation :: Vector -> Direction -> Vector
scaleRotation r@(V x y) (Left deg) = case deg of
    90 -> V (-y) x
    180 -> V (-x) (-y)
    270 -> V y (-x)
    _ -> r
scaleRotation r@(V x y) (Right deg) = case deg of
    90 -> V y (-x)
    180 -> V (-x) (-y)
    270 -> V (-y) x 
    _ -> r

manhattanDistance :: Vector -> Vector -> Int
manhattanDistance (V x y) (V x' y') = abs (x - x') + abs (y - y')

parseDirection :: String -> Direction
parseDirection value = case head value of
    'N' -> North num
    'S' -> South num
    'E' -> East num
    'W' -> West num
    'L' -> Left num
    'R' -> Right num
    'F' -> Forward num
    where num = read (tail value) :: Int

parseInput :: IO [Direction]
parseInput = do
    input <- readFile "12.in"
    return $ map parseDirection $ lines input

addDirection :: Vector -> Rotation -> Direction -> (Vector, Rotation)
addDirection p rot dir = case dir of
    North val -> (p + scaleV (V 0 1) val, rot)
    South val -> (p + scaleV (V 0 (-1)) val, rot)
    East val -> (p + scaleV (V (-1) 0) val, rot)
    West val -> (p + scaleV (V 1 0) val, rot)
    Left deg -> (p, scaleRotation rot dir)
    Right deg -> (p, scaleRotation rot dir)
    Forward val -> (p + scaleV rot val, rot)

processDirections :: Vector -> Vector -> [Direction] -> Vector
processDirections _ point [] = point
processDirections rot point (d:ds) = processDirections newRot newPoint ds
    where (newPoint, newRot) = addDirection point rot d

fstPart :: IO ()
fstPart = do
    directions <- parseInput
    print $ manhattanDistance (V 0 0) $ processDirections (V 1 0) (V 0 0) directions