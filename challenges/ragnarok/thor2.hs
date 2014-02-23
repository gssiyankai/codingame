import Control.Applicative
import Control.Monad
import System.IO
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    -- Read init information from standard input, if any
    initial_line <- getLine
    let thor_position = (map read (words initial_line))
    
    line <- getLine
    let strikes = (words line) !! 0
        nb_giants = (words line) !! 1
     
    lines <- replicateM (read nb_giants) getLine
    let giants_positions = map (\line -> map (\coord -> read coord) line)
                            $ map words lines
    hPutStrLn stderr strikes
    hPutStrLn stderr $ show giants_positions
        
    loop thor_position (read strikes) giants_positions


nearestGiant :: [Int] -> [[Int]] -> [Int]
nearestGiant thor_position@(tx:ty:_) giants_positions = head (sortBy sortGiantsByDistance giants_positions)
    where sortGiantsByDistance (g1x:g1y:_) (g2x:g2y:_) 
                | min_delta1 < min_delta2 = LT
                | min_delta1 > min_delta2 = GT
                | max_delta1 < max_delta2 = LT
                | max_delta1 > max_delta2 = GT
                | otherwise               = EQ
                where max_delta1 = max delta1_x delta1_y
                      max_delta2 = max delta2_x delta2_y
                      min_delta1 = min delta1_x delta1_y
                      min_delta2 = min delta2_x delta2_y
                      delta1_x = abs(g1x-tx)
                      delta1_y = abs(g1y-ty)
                      delta2_x = abs(g2x-tx)
                      delta2_y = abs(g2y-ty)
                      

isGiantInStrikeRange :: [Int] -> [Int] -> Bool
isGiantInStrikeRange (tx:ty:_) (gx:gy:_)
    | abs (tx - gx) > 9 = False
    | abs (ty - gy) > 9 = False
    | otherwise         = True
    
areAllGiansInStrikeRange :: [Int] -> [[Int]] -> Bool
areAllGiansInStrikeRange thor_position giants_positions = and $ map (\giant_position -> isGiantInStrikeRange thor_position giant_position) giants_positions

computeMove :: [Int] -> [[Int]] -> String
computeMove thor_position giants_positions = "W"
    
computeAction :: [Int] -> Int -> [[Int]] -> String
computeAction thor_position strikes giants_positions
    | shouldMove = computeMove thor_position giants_positions
    | shouldStrike = "STRIKE"
    | otherwise    = "WAIT"
    where shouldMove = hasEnoughStrikes && cannotStrikeAllGiants 
          hasEnoughStrikes = strikes <= length giants_positions
          cannotStrikeAllGiants = not $ areAllGiansInStrikeRange thor_position giants_positions
          shouldStrike = isGiantInStrikeRange thor_position (nearestGiant thor_position giants_positions)

computeNextThorPosition :: [Int] -> String -> [Int]
computeNextThorPosition (lx:ly:tx:ty:_) move = [lx,ly,tx+delta_x,ty+delta_y]
    where delta_y | isPrefixOf "S" move = 1
                  | isPrefixOf "N" move = -1
                  | otherwise           = 0
          delta_x | isSuffixOf "E" move = 1
                  | isSuffixOf "W" move = -1
                  | otherwise           = 0
    

loop :: [Int] -> Int -> [[Int]] -> IO ()
loop thor_position strikes giants_positions = do
    -- Read information from standard input

    -- Compute logic here
    let action = computeAction thor_position strikes giants_positions
        next_thor_position = computeNextThorPosition thor_position action
    
    -- hPutStrLn stderr "Debug messages..."

    -- Write action to standard output
    putStrLn action
    
    line <- getLine
    let strikes = (words line) !! 0
        nb_giants = (words line) !! 1
     
    lines <- replicateM (read nb_giants) getLine
    let giants_positions = map (\line -> map (\coord -> read coord :: Int) line)
                            $ map words lines
    hPutStrLn stderr strikes
    hPutStrLn stderr $ show giants_positions
        
    loop next_thor_position (read strikes) giants_positions
