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

bestMove :: [Move] -> Position -> GameState -> Move
bestMove [m] _ _ = m
bestMove moves p gs = maximumMoveScore
			$ map (\(move, next_p) -> (move, moveScore next_p gs))
			$ map (\move -> (move, nextPosition p move)) moves
	where
		maximumMoveScore [(m1,s1)] = m1
		maximumMoveScore [(m1,s1),(m2,s2)] = if(s1>=s2) then m1 else m2
		maximumMoveScore ((m1,s1):(m2,s2):mss) = if(s1>=s2) then maximumMoveScore ((m1,s1):mss)
								    else maximumMoveScore ((m2,s2):mss)
		maximum' [] = 0
		maximum' l = maximum l
		moveScore p gs = moveScore' p gs 0 []
		moveScore' _ _ 7 _ = 0
		moveScore' p gs i ps = 1 + (maximum' $ map (\next_p -> moveScore' next_p ((p:ps):gs) (i+1) (next_valid_moves++ps))
							next_valid_moves)
			where next_valid_moves = map (\(next_p,_) -> next_p)
						$ filter (\(next_p, ms) -> not (null ms))
						$ map (\next_p -> (next_p, validMoves moves next_p ((p:ps):gs)))
						$ filter (\(next_p) -> not (next_p `elem` ps))
						$ map (\move -> nextPosition p move) moves
