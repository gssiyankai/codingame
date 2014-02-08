module Tron where

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

