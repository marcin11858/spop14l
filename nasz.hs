import Control.Monad 
import Data.List
import Text.Read
import System.IO

--Type that defines position on board
data Position = Pos {x::Int, y::Int} deriving (Show)

data Tree = Empty  | Node {funValue::Double, recState::State, child::[Tree]} deriving (Show)

instance Eq Position where
                (Pos x1 y1) == (Pos x2 y2) = (x1 == x2) &&  (y1 == y2) 


--Global state - position of wolf and all sheeps
data State = State { wPosition::Position,
                     sPosition::[Position]} deriving (Show)
                     

-- Main function 
main =  do 
        printLine
        let state = getNewState
        play state

createTree::State->Int->Tree
createTree state depth = createTreeLevel (Node 0 state []) depth 0

createTreeLevel::Tree->Int->Int->Tree
createTreeLevel Empty _ _ = Empty
createTreeLevel _ 0 _ = Empty
createTreeLevel node depth position = Empty--if mod position 2 == 0 then
                                       --                      else

play :: State -> IO ()  
play state = do        
            printActualBoard state
--            input <- getLine
    --        putStrLn ( "INPUT " ++ input)
            if isFieldFreeWrapper state (Pos 8 1) then  putStrLn "Pole wolne"
                                            else  putStrLn "Pole zajęte"

            let wMoves = getPossibleSheepsMoves state
            do putStrLn (show wMoves)
            --let sMoves = getPossibleSheepMoves state
        --    case lookup input chooseOption of 
          --          Just action -> action state -- If it is we apply this action
            --        Nothing     -> putStrLn ( "INPUT " ++ input)
    
    


--Returns all available moves for wolf 
getPossibleWolfMoves :: State -> [Position]
getPossibleWolfMoves state = filter (isFieldFreeWrapper state) [(Pos a b) | a <- [possibleX-1, possibleX+1], b <- [possibleY-1, possibleY+1]]
                              where possibleX = (x (wPosition state))
                                    possibleY = (y (wPosition state)) -- Wolf can go backwards

                                    
getPossibleSheepsMoves :: State -> [Position]
getPossibleSheepsMoves state = getPossibleSheepMoves (sPosition state) state

getPossibleSheepMoves :: [Position] -> State -> [Position]
getPossibleSheepMoves [] _ = []
getPossibleSheepMoves (z:zs) state = (filter (isFieldFreeWrapper state)[(Pos a possibleY) | a <- [possibleX-1, possibleX+1]]) ++ (getPossibleSheepMoves zs state) 
                                     where possibleX = (x z)
                                           possibleY = (y z) + 1


-- Checks is position free                                           
isFieldFreeWrapper:: State->Position->Bool
isFieldFreeWrapper state pos = isFieldFree (wPosition state) (sPosition state) pos True
-- Checks is position free 
--Args: WolfPosition, [SheepPositions], position, True
isFieldFree :: Position->[Position]-> Position -> Bool -> Bool                                      
isFieldFree wPos [] pos answer = if (x pos)>0 && (x pos)<9 && (y pos)>0 && (y pos)<9 
                                    && pos/=wPos && answer
                                    then True
                                    else False    
isFieldFree wPos (s:sheeps) pos answer = isFieldFree wPos sheeps pos (s/=pos && answer)                                    
                                



chooseOption :: [(String, State -> IO ())]  
chooseOption =  [("n", newGame),
                 ("s", saveGame),
                 ("l", loadGame),
                 ("q", exitGame)]    

--Starts new Game 
newGame :: State -> IO() 
newGame state = main

--Starts new Game 
saveGame :: State -> IO() 
saveGame state = main

--Starts new Game 
loadGame :: State -> IO() 
loadGame state = main
--Ends a game
exitGame :: State -> IO() 
exitGame state = return ()


-- Function    returns initial state of game                 
getNewState :: State
getNewState = State (Pos 1 8) [Pos 2 1 ,Pos 4 1, Pos 6 1, Pos 8 1]     

-- Function    returns clear board - without wilf and sheeps
getClearBoard :: [[String]]
getClearBoard =   [[" ", "1", "2", "3", "4", "5", "6", "7", "8"],
                   ["1", "X", " ", "X", " ", "X", " ", "X", " "],
                   ["2", " ", "X", " ", "X", " ", "X", " ", "X"],
                   ["3", "X", " ", "X", " ", "X", " ", "X", " "],
                   ["4", " ", "X", " ", "X", " ", "X", " ", "X"],
                   ["5", "X", " ", "X", " ", "X", " ", "X", " "],
                   ["6", " ", "X", " ", "X", " ", "X", " ", "X"],
                   ["7", "X", " ", "X", " ", "X", " ", "X", " "],
                   ["8", " ", "X", " ", "X", " ", "X", " ", "X"]]
                   
                   
-- Function set state on board               
setBoard::Position->[Position]->[[String]]->[[String]]
setBoard pos [] tab = updateMatrix tab "W" (y pos, x pos)
setBoard pos (z:zs) tab = setBoard pos zs (updateMatrix tab "O" (y z, x z))

updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix m x (r,c) =  take r m ++  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++ drop (r + 1) m

-- PRINT FUNCTIONS

printLine :: IO()  
printLine = do putStrLn ""               
    
printMenu :: IO()      
printMenu = do
                putStrLn "WILK I OWCE"
                putStrLn "Menu:"
                putStrLn " n -> Nowa gra"
                putStrLn " l -> Wczytaj grę"
                putStrLn " s -> Zapisz grę"
                putStrLn " q -> Wyjdź z gry"
                
-- Function prints board on screen        
printBoard :: [[String]] -> IO ()
printBoard str = do
            mapM_ putStrLn [ b | b <- [unwords list_str | list_str <- str]]
        
-- Function prints boars with actual state         
printActualBoard ::State -> IO ()
printActualBoard state = printBoard (setBoard (wPosition state) (sPosition state) getClearBoard)