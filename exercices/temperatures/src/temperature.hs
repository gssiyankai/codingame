process ts = minimum $ map (\ t -> read t :: Int)
                     $ map (\ (t:ts) -> if (t == '-') then ts else (t:ts)) 
                     $ words ts

main :: IO ()
main = do
    n <- getLine
    case n of
        "0" -> print 0
        _ -> do
            ts <- getLine
            print $ process ts
