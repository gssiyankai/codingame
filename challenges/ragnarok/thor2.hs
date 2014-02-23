import Control.Monad
import System.IO
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    -- Read init information from standard input, if any
    thor_position <- do line <- getLine; return (map read (words line))
    
    loop thor_position


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
    | abs (tx - gx) > 5 = False
    | abs (ty - gy) > 5 = False
    | otherwise         = True
    
areAllGiansInStrikeRange :: [Int] -> [[Int]] -> Bool
areAllGiansInStrikeRange thor_position giants_positions = and $ map (\giant_position -> isGiantInStrikeRange thor_position giant_position) giants_positions

computeMove :: [Int] -> [[Int]] -> String
computeMove thor_position@(tx:ty:_) giants_positions = move
    where move = vertical_move ++ horizontal_move
          vertical_move | delta_y < 0 = "S"
                        | delta_y > 0 = "N"
                        | otherwise   = ""
          delta_y = ngy - ty
          horizontal_move | delta_x < 0 = "E"
                          | delta_x > 0 = "W"
                          | otherwise   = ""
          delta_x = ngx - tx
          nearest_giant = nearestGiant thor_position giants_positions
          ngx = nearest_giant !! 0
          ngy = nearest_giant !! 1
    
computeAction :: [Int] -> Int -> [[Int]] -> String
computeAction thor_position strikes giants_positions
    | shouldMove = move
    | shouldStrike = "STRIKE"
    | otherwise    = "WAIT"
    where shouldMove = hasNotEnoughStrikes && cannotStrikeAllGiants && isThorNextPositionInBounds
          hasNotEnoughStrikes = strikes <= length giants_positions
          move = computeMove thor_position giants_positions
          next_thor_position = computeNextThorPosition thor_position move
          isThorNextPositionInBounds
            | next_thor_position !! 0 >= 0 && next_thor_position !! 0 < 40 && 
              next_thor_position !! 1 >= 0 && next_thor_position !! 1 < 100    = True
            | otherwise                                                        = False
          cannotStrikeAllGiants = not $ areAllGiansInStrikeRange thor_position giants_positions
          shouldStrike = isGiantInStrikeRange thor_position (nearestGiant thor_position giants_positions)

computeNextThorPosition :: [Int] -> String -> [Int]
computeNextThorPosition positions "STRIKE" = positions 
computeNextThorPosition positions "WAIT" = positions
computeNextThorPosition (tx:ty:_) move = [tx+delta_x,ty+delta_y]
    where delta_y | isPrefixOf "S" move = 1
                  | isPrefixOf "N" move = -1
                  | otherwise           = 0
          delta_x | isSuffixOf "E" move = 1
                  | isSuffixOf "W" move = -1
                  | otherwise           = 0
    

loop :: [Int] -> IO ()
loop thor_position = do
    -- Read information from standard input
    line <- getLine
    let [strikes, nb_giants] = map read (words line)
     
    lines <- replicateM nb_giants getLine
    let giants_positions = map (\line -> map (\coord -> read coord) line)
                            $ map words lines
    hPutStrLn stderr $ show strikes
    hPutStrLn stderr $ show giants_positions
    

    -- Compute logic here
    let action = computeAction thor_position strikes giants_positions
        next_thor_position = computeNextThorPosition thor_position action
    
    -- hPutStrLn stderr "Debug messages..."

    -- Write action to standard output
    putStrLn action
        
    loop next_thor_position
