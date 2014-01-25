import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Read init information from standard input, if any
    line <- getLine
    info <- readInitInfo (read line :: Int)
    
    loop $ positions info

readInitInfo :: Int -> IO (String)
readInitInfo 0 = return ""
readInitInfo n = do
    line <- getLine
    nextLine <- readInitInfo (n-1)
    return (line ++ nextLine)

convert :: String -> [Int]
convert line = map (\ x -> read x :: Int) $ words line

positions :: String -> [(Int,Int)]
positions info = positions' $ convert info
    where positions' (x:y:ps) = (x,y) : positions' ps

params :: String -> IO ([Int])
params line = return (convert line)

rotation :: [Int] -> Int
rotation _ = 0

power :: [Int] -> Int
power (x:y:hs:vs:f:r:p)
    | abs vs >= 40  = 4
    | otherwise = 0
    

loop :: [(Int, Int)] -> IO ()
loop positions = do
    -- Read information from standard input
    line <- getLine
    
    -- Compute logic here
    params <- params line
    
    -- hPutStrLn stderr "Debug messages..."
    
    -- Write action to standard output
    putStrLn $ show (rotation params) ++ " " ++ show (power params)
    
    loop positions
    
