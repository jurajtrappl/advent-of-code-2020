module Util where
    import qualified Data.Text as T

    fromJust :: Maybe a -> a
    fromJust (Just v) = v

    iter :: (Eq t1, Num t1) => (t2 -> t2) -> t1 -> t2 -> t2
    iter f 0 x = x
    iter f n x = iter f (n-1) (f x)

    readInt :: String -> Int
    readInt x = read x :: Int

    readMaybe :: Read a => T.Text -> Maybe a
    readMaybe s = case reads (T.unpack s) of
                        [(val, "")] -> Just val
                        _           -> Nothing


    splitOn :: Foldable t => Char -> t Char -> [String]
    splitOn delim = foldr (\c (x:xs) -> if c == delim then "":x:xs else (c:x):xs) [""]

    tuplify :: [a] -> (a, a)
    tuplify [x, y] = (x, y)

    uncurry3 :: (t1 -> t2 -> t1) -> (t1, t2, t2) -> t1
    uncurry3 f (x, y, z) = f (f x y) z