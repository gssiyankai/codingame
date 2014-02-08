import Control.Applicative
import Control.Monad
import System.IO

type Move = String
type Position = (Int, Int)
type PlayerState = [Position]
type GameState = [PlayerState]

moves :: [Move]
moves = ["UP", "DOWN", "LEFT", "RIGHT"]

nextPosition :: Position -> Move -> Position
nextPosition (x,y) move
	| move == "UP" 	  = (x,y-1)
	| move == "DOWN"  = (x,y+1)
	| move == "LEFT"  = (x-1,y)
	| move == "RIGHT" = (x+1,y)
	
validMoves :: [Move] -> Position -> GameState -> [Move]
validMoves moves p gs = map (\(move, _) -> move)
			$ filter (\(_, p) -> validPosition p)
			$ map (\move -> (move, nextPosition p move)) moves
	where
		validPosition p@(x,y) = validX x && validY y && unoccupiedPosition p
		validX x = x>=0 && x<30
		validY y = y>=0 && y<20
		unoccupiedPosition p = null $ dropWhile (\ps -> not (p `elem` ps)) gs


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    -- Read init information from standard input, if any
    line <- getLine;
    let n = read $ (words line) !! 0
        p = read $ (words line) !! 1;
        
    loop (replicate n []) n p

loop :: GameState -> Int -> Int-> IO ()
loop previous_game_state n p = do
    -- Read information from standard input
    players_positions <- replicateM n ((\(x0:y0:x1:y1:_) -> (x1,y1)) . map read . words <$> getLine)
    
    let game_state = map (\(current_position,previous_positions) -> if current_position==(-1,-1) then [] else current_position:previous_positions)
                   $ zip players_positions previous_game_state
        current_position =  players_positions !! p;
    
    -- Compute logic here
    let valid_moves = validMoves moves current_position game_state
    
    -- hPutStrLn stderr "Debug messages..."
    
    -- Write action to standard output
    putStrLn $ head valid_moves
    
    line <- getLine;
    
    loop game_state n p

