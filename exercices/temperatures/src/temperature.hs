import Data.List

process ts = head $ sortBy closestToZero
                  $ map (read :: String -> Int)
                  $ words ts
        where closestToZero a b 
                        | a < 0     = closestToZero b (-a)
                        | b < 0     = closestToZero a (-b)
                        | a <= b    = LT
                        | otherwise = GT

main :: IO ()
main = do
    n <- getLine
    case n of
        "0" -> print 0
        _ -> do
            ts <- getLine
            print $ process ts
