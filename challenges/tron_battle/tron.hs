module Tron where

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
