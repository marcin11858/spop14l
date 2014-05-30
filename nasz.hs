import Control.Monad 
import Data.List
import Text.Read
import System.IO

-- Main function 
main =  do 
		let board = getNewBoard
		let state = getNewState
		let board2 = setBoard (wPosition state) (sPosition state) board
		printBoard board2
  --  newGameMsg
    -- state = getNewState

	
	--Type for position
data Position = Position {x::Int, y::Int}
data State = State { wPosition::Position,
					 sPosition::[Position]}
	
getNewState :: State
getNewState = State (Position 8 1) [Position 1 2 ,Position 1 4, Position 1 6, Position 1 8]	 

--printBoard :: State->[[String]]


getNewBoard :: [[String]]
getNewBoard = 	  [[" ", "1", "2", "3", "4", "5", "6", "7", "8"],
                   ["1", "X", " ", "X", " ", "X", " ", "X", " "],
                   ["2", " ", "X", " ", "X", " ", "X", " ", "X"],
                   ["3", "X", " ", "X", " ", "X", " ", "X", " "],
                   ["4", " ", "X", " ", "X", " ", "X", " ", "X"],
                   ["5", "X", " ", "X", " ", "X", " ", "X", " "],
                   ["6", " ", "X", " ", "X", " ", "X", " ", "X"],
                   ["7", "X", " ", "X", " ", "X", " ", "X", " "],
                   ["8", " ", "X", " ", "X", " ", "X", " ", "X"]
                   ]
				   
setBoard::Position->[Position]->[[String]]->[[String]]
setBoard pos [] tab = updateMatrix tab "W" (x pos, y pos)
setBoard pos (z:zs) tab = setBoard pos zs (updateMatrix tab "O" (x z, y z))

{-
setOnBoard board "W" wPosition
setOnBoard::[[String]]->String->Position->[[String]]
setOnBoard board symbol position = 
-}



updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix m x (r,c) =  take r m ++  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++ drop (r + 1) m

			   
				   
				   
	--Print board                
printBoard :: [[String]] -> IO ()
printBoard str = do
            mapM_ putStrLn [ b | b <- [unwords list_str | list_str <- str]]