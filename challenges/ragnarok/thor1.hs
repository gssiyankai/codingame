import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    -- Read init information from standard input, if any
    positions <- getLine
    loop (map read (words positions))

computeMove :: [Int] -> (String, [Int])
computeMove (lx:ly:tx:ty:_)  = (move, next_positions)
    where move = vertical_move ++ horizontal_move
          vertical_move | delta_y > 0 = "S"
                        | delta_y < 0 = "N"
                        | otherwise   = ""
          horizontal_move | delta_x > 0 = "E"
                          | delta_x < 0 = "W"
                          | otherwise   = ""
          delta_y | ly - ty > 0 = 1
                  | ly - ty < 0 = -1
                  | otherwise   = 0
          delta_x | lx - tx > 0 = 1
                  | lx - tx < 0 = -1
                  | otherwise   = 0
          next_positions = [lx,ly,tx+delta_x,ty+delta_y]

loop :: [Int] -> IO ()
loop positions = do
    -- Read information from standard input
    power <- getLine
    
    -- Compute logic here
    let (move, next_positions) = computeMove positions
    
    -- hPutStrLn stderr "Debug messages..."
    hPutStrLn stderr $ show positions
    hPutStrLn stderr $ show next_positions
    
    -- Write action to standard output
    putStrLn move
    
    loop next_positions
