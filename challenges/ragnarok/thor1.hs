import Control.Applicative
import Control.Monad
import System.IO
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    -- Read init information from standard input, if any
    positions <- getLine
    loop (map read (words positions))

computeMove :: [Integer] -> String
computeMove (lx:ly:tx:ty:_)  = vertical_move ++ horizontal_move
    where vertical_move | delta_y > 0 = "S"
                        | delta_y < 0 = "N"
                        | otherwise   = ""
          delta_y = ly - ty
          horizontal_move | delta_x > 0 = "E"
                          | delta_x < 0 = "W"
                          | otherwise   = ""
          delta_x = lx - tx
          
computeNextPositions :: [Integer] -> String -> [Integer]
computeNextPositions (lx:ly:tx:ty:_) move = [lx,ly,tx+delta_x,ty+delta_y]
    where delta_y | isPrefixOf "S" move = 1
                  | isPrefixOf "N" move = -1
                  | otherwise           = 0
          delta_x | isSuffixOf "E" move = 1
                  | isSuffixOf "W" move = -1
                  | otherwise           = 0

loop :: [Integer] -> IO ()
loop positions = do
    -- Read information from standard input
    power <- getLine
    
    -- Compute logic here
    let move = computeMove positions
        next_positions = computeNextPositions positions move
    
    -- hPutStrLn stderr "Debug messages..."
    hPutStrLn stderr $ show positions
    hPutStrLn stderr $ show next_positions
    
    -- Write action to standard output
    putStrLn move
    
    loop next_positions
