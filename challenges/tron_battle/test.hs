import Tron
import Test.HUnit

test0 = TestCase $ do
	assertEqual 
  		"Initial moves" ["UP", "DOWN", "LEFT", "RIGHT"] moves

test1 = TestCase $ do
	assertEqual 
  		"Compute next position when moving up" (5, 8) (nextPosition (5, 9) "UP")
	assertEqual
  		"Compute next position when moving down" (1, 2) (nextPosition (1, 1) "DOWN")
	assertEqual
  		"Compute next position when moving left" (6, 3) (nextPosition (7, 3) "LEFT")
	assertEqual
  		"Compute next position when moving right" (4, 7) (nextPosition (3, 7) "RIGHT")

test2 = TestCase $ do
	assertEqual 
  		"Don't move out of limits" ["UP", "DOWN", "LEFT", "RIGHT"] (validMoves moves (1, 1) [[]])
	assertEqual 
  		"Don't move out of limits" ["DOWN", "RIGHT"] (validMoves moves (0, 0) [[]])
	assertEqual 
  		"Don't move on previous position" ["RIGHT"] (validMoves moves (0, 0) [[(0,1)]])

test3 = TestCase $ do
-- O O . |
-- . X . |
-- | | | |
	assertEqual 
  		"Don't go to dead ends" "RIGHT" (bestMove ["LEFT", "RIGHT"] (1, 1) [[(1,1),(0,1),(0,0)],[(3,0),(3,1),(0,2),(1,2),(2,2),(3,2)]])
	

main = runTestTT $ TestList
			[
				test0,
				test1,
				test2,
				test3
			]
