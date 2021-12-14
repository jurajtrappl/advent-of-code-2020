import Data.List.Split (splitWhen)
import Data.Bifunctor (Bifunctor(bimap))

type Vertex = String
type Queue = [Vertex]
type UnweightedGraph = [(Vertex, [Vertex])]
type WeightedEdge = (Vertex, Int)
type Queue' = [WeightedEdge]
type WeightedGraph = [(Vertex, [WeightedEdge])]

tuplify :: [a] -> (a, a)
tuplify [x, y] = (x, y)

toVertex :: [String] -> Vertex
toVertex = concat . take 2

unsafeReadInt :: String -> Int
unsafeReadInt s = read s :: Int

parseEdge :: [String] -> [WeightedEdge]
parseEdge [] = []
parseEdge ["no","other","bags."] = []
parseEdge e = (concat (take 2 (drop 1 e)), unsafeReadInt $ head e) : parseEdge (drop 4 e)

parseInput :: IO WeightedGraph
parseInput = do
    fContent <- readFile "07.in"
    let splitted = map (splitWhen (== "contain") . words) (lines fContent)
    return $ map (bimap toVertex parseEdge . tuplify) splitted

getNeighbours :: WeightedGraph -> Vertex -> [WeightedEdge]
getNeighbours g v = snd $ head $ filter (\(v', _) -> v' == v) g

isShinyGoldReachable :: WeightedGraph -> Queue -> [Vertex] -> Bool
isShinyGoldReachable _ [] _ = False
isShinyGoldReachable g (current:qs) visited
    | current == "shinygold" = True
    | otherwise = isShinyGoldReachable g newQueue (visited ++ [current])
    where neighbours = map fst $ getNeighbours g current
          newQueue = qs ++ filter (`notElem` visited) neighbours

fstPart :: IO ()
fstPart = do
    g <- parseInput
    let vertices = filter (/= "shinygold") $ map fst g
    print $ length $ filter (== True) $ map (\v -> isShinyGoldReachable g [v] []) vertices



