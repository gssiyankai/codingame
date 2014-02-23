import Control.Monad
import System.IO
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    -- Read init information from standard input, if any
    thor_position <- do line <- getLine; return (map read (words line))
    
    loop thor_position

distance :: [Int] -> [Int] -> Int
distance (tx:ty:_) (gx:gy:_) = square (tx-gx) + square (ty-gy)
    where square x = x * x

nearestGiant :: [Int] -> [[Int]] -> [Int]
nearestGiant thor_position@(tx:ty:_) giants_positions = 
    snd $ head $ sort $ map (\giant_position -> (distance thor_position giant_position, giant_position)) giants_positions
                      
isGiantInStrikeRange :: [Int] -> [Int] -> Bool
isGiantInStrikeRange (tx:ty:_) (gx:gy:_)
    | abs (tx - gx) > 5 = False
    | abs (ty - gy) > 5 = False
    | otherwise         = True
    
areAllGiantsInStrikeRange :: [Int] -> [[Int]] -> Bool
areAllGiantsInStrikeRange thor_position giants_positions = and $ map (\giant_position -> isGiantInStrikeRange thor_position giant_position) giants_positions

isThorOutOfGiantRange :: [Int] -> [Int] -> Bool
isThorOutOfGiantRange (tx:ty:_) (gx:gy:_) = abs(tx-gx) > 1 || abs(ty-gy)>1

isThorOutOfAllGiantsRange :: [Int] -> [[Int]] -> Bool
isThorOutOfAllGiantsRange thor_position giants_positions = and $ map (\giant_position -> isThorOutOfGiantRange thor_position giant_position) giants_positions

computeMove :: [Int] -> [[Int]] -> String
computeMove thor_position@(tx:ty:_) giants_positions = move 
    where move = vertical_move ++ horizontal_move
          vertical_move | ty+1<18 && delta_y < 0 = "S"
                        | ty-1>=0 && delta_y > 0 = "N"
                        | otherwise   = ""
          delta_y = ngy-ty
          horizontal_move | tx+1<40 && delta_x < 0 = "E"
                          | tx-1>=0 && delta_x > 0 = "W"
                          | otherwise   = ""
          delta_x = ngx-tx
          [ngx, ngy] = nearestGiant thor_position giants_positions
    
computeAction :: [Int] -> Int -> [[Int]] -> String
computeAction thor_position strikes giants_positions
    | shouldMove && isMoveValid = move
    | shouldStrike = "STRIKE"
    | otherwise    = "WAIT"
    where shouldMove = hasNotEnoughStrikes && cannotStrikeAllGiants
          hasNotEnoughStrikes = strikes <= length giants_positions
          cannotStrikeAllGiants = not $ areAllGiantsInStrikeRange thor_position giants_positions
          next_thor_position = computeNextThorPosition thor_position move
          isThorNextPositionInBounds = next_thor_position !! 0 >= 0 && next_thor_position !! 0 < 40 && 
                                       next_thor_position !! 1 >= 0 && next_thor_position !! 1 < 18
          isThorNextPositionOutOfGiantsRange = isThorOutOfAllGiantsRange next_thor_position giants_positions
          isMoveValid = isThorNextPositionInBounds && isThorNextPositionOutOfGiantsRange
          move = computeMove thor_position giants_positions
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
    hPutStrLn stderr $ show thor_position
    hPutStrLn stderr $ show strikes
    hPutStrLn stderr $ show giants_positions
    hPutStrLn stderr $ show $ nearestGiant thor_position giants_positions
    hPutStrLn stderr $ show $ computeMove thor_position giants_positions
    

    -- Compute logic here
    let action = computeAction thor_position strikes giants_positions
        next_thor_position = computeNextThorPosition thor_position action
    
    -- hPutStrLn stderr "Debug messages..."

    -- Write action to standard output
    putStrLn action
        
    loop next_thor_position
