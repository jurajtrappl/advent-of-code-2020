module T07 where
    import qualified Data.Graph as G
    import qualified Data.Text as T
    import qualified Data.Text.IO as IO
    import Util (readMaybe, split')
    
    parseInput :: IO [T.Text]
    parseInput = do
        input <- IO.readFile "07.in"
        return $ T.lines input

    emptyEdge :: (Int, T.Text)
    emptyEdge = (0, T.pack "")

    parseEdge :: T.Text -> (Int, T.Text)
    parseEdge value = case weight of
            Just w -> (w, color)
            Nothing -> emptyEdge
        where weight = readMaybe (head (T.words value))
              color = T.unwords $ take 2 $ drop 1 $ T.words value

    processLine :: [T.Text] -> (T.Text, [(Int, T.Text)])
    processLine [unprocOrigin, unprocEdges] = (originColor, if edges == [emptyEdge] then [] else edges)
        where originColor = T.unwords $ take 2 (T.words unprocOrigin)
              edges = map parseEdge (split' unprocEdges ",")

    parseGraph :: [T.Text] -> [(T.Text, [(Int, T.Text)])]
    parseGraph input = map processLine splitted
        where splitted = map (`split'` "contain") input

    createUnweightedGraph :: [(T.Text, [(Int, T.Text)])] -> [(T.Text, Int)] -> G.Graph
    createUnweightedGraph d vIds = G.buildG (snd $ head vIds, snd $ last vIds) $ concatMap createEdges onlyIds
        where edgesIds = map (concatMap (numberEdge vIds) . snd) d
              zipped = zip vIds edgesIds
              onlyIds = zip (map (snd . fst) zipped) (map (map snd . snd) zipped)
              
    numberVertices :: [T.Text] -> [(T.Text, Int)]
    numberVertices vertices = zip vertices [1..length vertices]

    numberEdge :: [(T.Text, Int)] -> (Int, T.Text) -> [(T.Text, Int)]
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
        let shinyGoldId = snd $ head $ filter (\ (n,_) -> n == T.pack "shiny gold") vIds
        let allReachable = map (\ (color, id) -> filter (/=id) $ G.reachable g id) vIds
        return $ length $ filter (elem shinyGoldId) allReachable


