import Control.Monad
import System.IO
import Data.List

max_x = 40
max_y = 18

moves :: [String]
moves = ["N", "NE", "E", "SE", "S", "SW", "W", "NW"]

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

isThorPositionInBounds :: [Int] -> Bool
isThorPositionInBounds (tx:ty:_) = tx>=0 && tx<max_y && ty>=0 && ty<max_y

computeMove :: [Int] -> [[Int]] -> String
computeMove thor_position@(tx:ty:_) giants_positions 
    | null possible_moves = ""
    | otherwise           = head possible_moves
    where possible_moves = map fst 
                            -- $ map (\(move,next_thor_position) -> (move, distance [max_x `div` 2, max_y `div` 2] next_thor_position))
                            $ filter (\(_,next_thor_position) -> isThorOutOfAllGiantsRange next_thor_position giants_positions)
                            $ filter (\(_,next_thor_position) -> isThorPositionInBounds next_thor_position)
                            $ map (\move -> (move, computeNextThorPosition thor_position move)) moves 

computeAction :: [Int] -> Int -> [[Int]] -> String
computeAction thor_position strikes giants_positions
    | shouldMove && canMove = move
    | shouldStrike = "STRIKE"
    | otherwise    = "WAIT"
    where shouldMove = (hasNotEnoughStrikes || isThorPositionInGiantsRange) && cannotStrikeAllGiants
          hasNotEnoughStrikes = strikes <= length giants_positions
          cannotStrikeAllGiants = not $ areAllGiantsInStrikeRange thor_position giants_positions
          isThorPositionInGiantsRange = not $ isThorOutOfAllGiantsRange thor_position giants_positions
          move = computeMove thor_position giants_positions
          canMove = not $ null move
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
