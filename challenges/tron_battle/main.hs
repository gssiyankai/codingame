import Control.Applicative
import Control.Monad
import System.IO

type Move = String
type Position = (Int, Int)

moves :: [Move]
moves = ["UP", "DOWN", "LEFT", "RIGHT"]

nextPosition :: Position -> Move -> Position
nextPosition (x,y) move
	| move == "UP" 	  = (x,y-1)
	| move == "DOWN"  = (x,y+1)
	| move == "LEFT"  = (x-1,y)
	| move == "RIGHT" = (x+1,y)
	
validMoves :: [Move] -> Position -> [Position] -> [Move]
validMoves moves p ps = map (\(move, _) -> move)
				$ filter (\(_, p@(x,y)) -> x>=0 && x<30 && y>=0 && y<20
							&& not (p `elem` ps))
				$ map (\move -> (move, nextPosition p move)) moves

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    -- Read init information from standard input, if any
    loop []

loop :: [Position] -> IO ()
loop previous_positions = do
    -- Read information from standard input
    line <- getLine;
    let n = read $ (words line) !! 0
        p = read $ (words line) !! 1;

    players_positions <- replicateM n (map read . words <$> getLine)
    
    let current_position = (\(x0:y0:x1:y1:_) -> (x1,y1)) $ players_positions !! p
    
    -- Compute logic here
    let valid_moves = validMoves moves current_position previous_positions
    
    -- hPutStrLn stderr "Debug messages..."
    
    -- Write action to standard output
    putStrLn $ head valid_moves
    
    loop (current_position : previous_positions)
