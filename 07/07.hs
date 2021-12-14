import qualified Data.Graph as Graph
import qualified Data.Text as Text
import qualified Data.Text.IO as IO

type Rule = Text.Text

readMaybe :: Read a => Text.Text -> Maybe a
readMaybe s = case reads (Text.unpack s) of
                    [(val, "")] -> Just val
                    _           -> Nothing

parseInput :: IO [Rule]
parseInput = fmap Text.lines (IO.readFile "07.in")

emptyEdge :: (Int, Text.Text)
emptyEdge = (0, Text.pack "")

parseEdge :: Text.Text -> (Int, Text.Text)
parseEdge value = case weight of
        Just w -> (w, color)
        Nothing -> emptyEdge
    where weight = readMaybe (head (Text.words value))
          color = Text.unwords $ take 2 $ drop 1 $ Text.words value

split' :: Text.Text -> String -> [Text.Text]
split' value del = Text.splitOn (Text.pack del) value

processLine :: [Text.Text] -> (Text.Text, [(Int, Text.Text)])
processLine [unprocOrigin, unprocEdges] = (originColor, if edges == [emptyEdge] then [] else edges)
    where originColor = Text.unwords $ take 2 (Text.words unprocOrigin)
          edges = map parseEdge (split' unprocEdges ",")

parseGraph :: [Text.Text] -> [(Text.Text, [(Int, Text.Text)])]
parseGraph input = map processLine splitted
    where splitted = map (`split'` "contain") input

createUnweightedGraph :: [(Text.Text, [(Int, Text.Text)])] -> [(Text.Text, Int)] -> Graph.Graph
createUnweightedGraph d vIds = Graph.buildG (snd $ head vIds, snd $ last vIds) $ concatMap createEdges onlyIds
    where edgesIds = map (concatMap (numberEdge vIds) . snd) d
          zipped = zip vIds edgesIds
          onlyIds = zip (map (snd . fst) zipped) (map (map snd . snd) zipped)
            
numberVertices :: [Text.Text] -> [(Text.Text, Int)]
numberVertices vertices = zip vertices [1..length vertices]

numberEdge :: [(Text.Text, Int)] -> (Int, Text.Text) -> [(Text.Text, Int)]
numberEdge vIds (_, v) = found
    where found = filter ((== v) . fst) vIds

createEdges :: (Int, [Int]) -> [(Int, Int)]
createEdges (v, n) = [ (v, a) | a <- n ]

fstPart :: IO Int
fstPart = do
    input <- parseInput
    let unprocG = parseGraph input
    let vIds = numberVertices $ map fst unprocG
    let g = createUnweightedGraph unprocG vIds
    let shinyGoldId = snd $ head $ filter (\ (n,_) -> n == Text.pack "shiny gold") vIds
    let allReachable = map (\ (color, id) -> filter (/=id) $ Graph.reachable g id) vIds
    return $ length $ filter (elem shinyGoldId) allReachable


