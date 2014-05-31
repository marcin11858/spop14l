import Control.Monad 
import Data.List
import Data.Ord
import Text.Read
import System.IO

--Type that defines position on board
data Position = Pos {x::Int, y::Int} deriving (Show)

data Tree = Tree {state::State, tree::[Tree], funValue::Int} deriving (Show)

instance Eq Position where
                (Pos x1 y1) == (Pos x2 y2) = (x1 == x2) &&  (y1 == y2) 


--Global state - position of wolf and all sheeps
data State = State { wPosition::Position,
                     sPosition::[Position]} deriving (Show)
                     

-- Main function 
main =  do 
        printLine
        
        let state = getNewState
        let tree1 = createTree state 1
        let oceny = getChildValues(tree1)
        putStrLn (show oceny)
        --putStrLn (show tree1)
        printLine
 --       printTree 0 tree1 
        play state

        let tree10 = createTree state 3
        let tree2 = minmax 0 tree10
        printTree 0 tree2
        printActualBoard state
        putStrLn (show (evaluateState state ))
        --play state
-- Tworzenie drzewa

createTree::State->Int->Tree
createTree state depth = createTreeLevel depth 0 state 

createTreeLevel::Int->Int->State->Tree -- drzewo rodzic, glebokosc drzewa, 
createTreeLevel 0 _ state = Tree  state [] (evaluateState state)
createTreeLevel depth position state =  Tree  state (map (createTreeLevel (depth-1) (position+1)) (getNextStates state position)) (-1)


minmax::Int->Tree->Tree
minmax _ (Tree  state [] fValue) = Tree  state [] (evaluateState state)
minmax depth (Tree  state tree fValue) = if mod depth 2 == 0 then Tree  state (map (minmax (depth+1)) tree) (maximum (map getValue tree) )
                                                             else Tree  state (map (minmax (depth+1)) tree) (minimum (map getValue tree) )
{-
minmax2::Int->Tree->(Int, State)
minmax2 _ (Tree  state [] fValue) = ((evaluateState state), state)
minmax2 depth (Tree  state tree fValue) = if mod depth 2 == 0 then findMax
                                                              else findMin
-}
                                                              
getMaximumTuple::[(Int, State)]->(Int, State)
getMaximumTuple list = maximumBy (comparing fst) list
{-
play::GameTree->Int
play (GameTree p []) = evalState p
play (GameTree (White,_) xs) = maximum (map play xs)
play (GameTree (Black,_) xs) = minimum (map play xs)                               
-}



getChildValues::Tree->[Int]                               
getChildValues (Tree  state tree fValue)= map getValue tree

getValue::Tree->Int
getValue tree = (funValue tree)


getNextStates::State->Int->[State]
getNextStates state position = if mod position 2 == 0 then getPossibleSheepsStates (sPosition state) state
                                                      else getPossibleWolfStates state 
-- Funkcja celu dla drzewa
evaluateState::State->Int
evaluateState state = 10000 -  alpha1 * (sheepDistribution (sPosition state)) + alpha2 * (wolfNeighborhood (wPosition state) (sPosition state)) + alpha3 * ( y (wPosition state))
                        where alpha1 = -4
                              alpha2 = -1
                              alpha3 = 4

sheepDistribution::[Position]->Int
sheepDistribution positions = (maxSheepY positions) - (minSheepY positions)

maxSheepY::[Position]->Int
maxSheepY [] = 0
maxSheepY (z:zs) = max (y z) (maxSheepY zs)

minSheepY::[Position]->Int
minSheepY [] = 9999999999999
minSheepY (z:zs) = min (y z) (minSheepY zs) 

wolfNeighborhood::Position->[Position]->Int
wolfNeighborhood _ [] = 0
wolfNeighborhood wolfPos (z:zs) = (abs ((x wolfPos) - ( x z) )) + (abs ((y wolfPos) - ( y z) )) + (wolfNeighborhood wolfPos zs)                       

--   
 
getFakeSheepsStates::State->[State]
getFakeSheepsStates _ = [State (Pos 1 8) [Pos 2 1 ,Pos 4 1, Pos 6 1, Pos 8 1], State (Pos 1 8) [Pos 2 1 ,Pos 4 1, Pos 6 1, Pos 7 2]]

getFakeWolfStates::State->[State] 
getFakeWolfStates _ = [State (Pos 1 8) [Pos 2 1 ,Pos 4 1, Pos 6 1, Pos 8 1], State (Pos 2 7) [Pos 2 1 ,Pos 4 1, Pos 6 1, Pos 8 1]]

play :: State -> IO ()  
play state = do        
            printActualBoard state
            
--            input <- getLine
    --        putStrLn ( "INPUT " ++ input)
            if isFieldFreeWrapper state (Pos 8 1) then  putStrLn "Pole wolne"
                                            else  putStrLn "Pole zajęte"



            
            
            
            let state2=moveSheep state (Pos 4 1) (Pos 4 3)
            let wMoves = getPossibleWolfMoves state2
            
            let state2=moveWolf state (Pos 1 2)
            printActualBoard state2

            let wMoves = getPossibleSheepsMoves state
            
            let sStates = getPossibleSheepsStates (sPosition state) state
            let sWolfs  = getPossibleWolfStates state2
            --printStates sStates
            printStates sWolfs
            

            --let sMoves = getPossibleSheepMoves state
        --    case lookup input chooseOption of 
          --          Just action -> action state -- If it is we apply this action
            --        Nothing     -> putStrLn ( "INPUT " ++ input)
   
printStates::[State] -> IO () 
printStates [] = putStrLn "KONIEC----------------------------------"
printStates (s:states)= do  putStrLn "---------------------------------------"
                            printState s
                            putStrLn "---------------------------------------"
                            printStates states
             
printState::State -> IO () 
printState state = do   putStr "W:("  
                        putStr (show(x (wPosition state))) 
                        putStr ","     
                        putStr (show(y (wPosition state))) 
                        putStr ") O:[" 
                        printSheepsPos (sPosition state)
                        putStr "]" 
                        
printSheepsPos::[Position] -> IO () 
printSheepsPos [] = putStr ""                  
printSheepsPos (p:pos)  = do    putStr"("
                                putStr (show(x p)) 
                                putStr ","     
                                putStr (show(y p)) 
                                putStr ")"
                                printSheepsPos pos


       
printTree::Int->Tree -> IO () 
printTree depth (Tree  state [] fValue)  = do   
                                            putStr " Poziom: "
                                            putStr (show depth)
                                            putStr " Ocena: "
                                            putStr (show fValue)
                                            putStr "   "
                                            printState state
                                            putStrLn ""
printTree depth (Tree  state tree fValue)  = 
                                         do 
                                            putStr " Poziom: "
                                            putStr (show depth)
                                            putStr " Ocena: "
                                            putStr (show fValue)
                                            putStr "   "
                                            printState state
                                            putStrLn "" 
                                            (mapM_ (printTree (depth+1)) tree )


{-
preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node a l r) = [a] ++ preorder l ++ preorder r
-}

--Returns all available moves for wolf 
getPossibleWolfMoves :: State -> [Position]
getPossibleWolfMoves state = filter (isFieldFreeWrapper state) [(Pos a b) | a <- [possibleX-1, possibleX+1], b <- [possibleY-1, possibleY+1]]
                              where possibleX = (x (wPosition state))
                                    possibleY = (y (wPosition state)) -- Wolf can go backwards

                                    
getPossibleWolfStates::State->[State]
getPossibleWolfStates state = (map (moveWolf state) (filter (isFieldFreeWrapper state) [(Pos a b) | a <- [possibleX-1, possibleX+1], b <- [possibleY-1, possibleY+1]]))
                              where possibleX = (x (wPosition state))
                                    possibleY = (y (wPosition state)) -- Wolf can go backwards                        
                                    
                                    
                                    
                                    
                                    
getPossibleSheepsMoves :: State -> [Position]
getPossibleSheepsMoves state = getPossibleSheepMoves (sPosition state) state

getPossibleSheepMoves :: [Position] -> State -> [Position]
getPossibleSheepMoves [] _ = []
getPossibleSheepMoves (z:zs) state = (filter (isFieldFreeWrapper state)[(Pos a possibleY) | a <- [possibleX-1, possibleX+1]]) ++ (getPossibleSheepMoves zs state) 
                                     where possibleX = (x z)
                                           possibleY = (y z) + 1

                                           -- pozycje owiec, stan, zwraca możliwe stany
getPossibleSheepsStates::[Position]->State->[State]
getPossibleSheepsStates [] _ = []
getPossibleSheepsStates (z:zs) state = (map (moveSheep state z) (filter (isFieldFreeWrapper state)[(Pos a ((y z) + 1)) | a <- [(x z)-1, (x z)+1]]))
                                            ++ (getPossibleSheepsStates zs state)


                                           
-- Moves sheep from old position to new position
moveSheep :: State -> Position -> Position -> State
moveSheep state oldPos newPos = State (wPosition state) (sheepsPosition) 
                                    where sheepsPosition = moveSheepFromTo (sPosition state) oldPos newPos
 
moveSheepFromTo :: [Position] -> Position -> Position -> [Position]
moveSheepFromTo (s:sheeps) oldPos newPos = if s==oldPos then newPos : sheeps
                                                   else s : (moveSheepFromTo sheeps oldPos newPos)

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
getNewState = State (Pos 2 7) [Pos 2 1 ,Pos 4 1, Pos 6 1, Pos 8 1]     

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


          
-- Moves wolf to position
moveWolf :: State -> Position -> State
moveWolf state newWPosition = State (newWPosition) (sPosition state)



{-
--Wrapper function for applying new positions of sheep after loading the file
putSheep :: [[String]] -> [Int] -> [[String]]
putSheep board newSheepPositions = applySheep board newSheepPositions currentSheepPossitions
                                    where currentSheepPossitions = getAllSheep board -- gets current sheep positions

--This function applies new sheep positions.
--For each pair (current position and new position) it simply applies move and then the result board is returned                                    
applySheep :: [[String]] -> [Int] -> [(Int, Int)] -> [[String]]
applySheep board [] _ = board
applySheep board _ [] = board
applySheep board (y:x:xs) (n:ns) = applySheep(applyMove board n(y,x)) xs ns -- recursion to apply new position of sheep
-}
