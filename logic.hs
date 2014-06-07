module Logic  where

import Control.Monad 
import Data.List
import Data.Ord
import Text.Read
import System.IO
import System.Directory

data Position = Pos {x::Int, y::Int} deriving (Show)

data Tree = Tree {state::State, tree::[Tree], funValue::Int} deriving (Show)

instance Eq Position where
                (Pos x1 y1) == (Pos x2 y2) = (x1 == x2) &&  (y1 == y2) 

data State = State { wPosition::Position,
                     sPosition::[Position]} deriving (Show)

findAndMakeSheepMove::State->State
findAndMakeSheepMove state = do
                             let tree = createTree state 6
                             snd (minmax 0 tree)

-- Tworzenie drzewa

createTree::State->Int->Tree
createTree state depth = createTreeLevel depth 0 state 

createTreeLevel::Int->Int->State->Tree -- drzewo rodzic, glebokosc drzewa, 
createTreeLevel 0 _ state = Tree  state [] (evaluateState state)
createTreeLevel depth position state =  Tree  state (map (createTreeLevel (depth-1) (position+1)) (getNextStates state position)) (-1)



minmax::Int->Tree->(Int, State)
minmax _ (Tree  state [] fValue) = ((evaluateState state), state)
minmax 0 (Tree  state tree fValue) =  getMaximumTuple (map (minmax (1)) tree)                                                        
minmax depth (Tree  state tree fValue) = if mod depth 2 == 0 then (fst (getMaximumTuple (map (minmax (depth + 1)) tree)), state)
                                                              else (fst (getMinimumTuple (map (minmax (depth + 1)) tree)), state)

                                                            
getMaximumTuple::[(Int, State)]->(Int, State)
getMaximumTuple list = maximumBy (comparing fst) list

getMinimumTuple::[(Int, State)]->(Int, State)
getMinimumTuple list = minimumBy (comparing fst) list

getNextStates::State->Int->[State]
getNextStates state position = if mod position 2 == 0 then getPossibleSheepsStates (sPosition state) state
                                                      else getPossibleWolfStates state 
-- Funkcja celu dla drzewa
evaluateState::State->Int
evaluateState state =  alpha1 * (sheepDistribution (sPosition state)) + alpha2 * (wolfNeighborhood (wPosition state) (sPosition state)) + alpha3 * ( y (wPosition state)) + alpha4 * (getCountPossibleMoves state)
                        where alpha1 = -4
                              alpha2 = -1
                              alpha3 = 4
                              alpha4 = 4

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

getCountPossibleMoves::State->Int
getCountPossibleMoves state = if length (getPossibleWolfMoves state) ==  0 then 99 else 4 -  length (getPossibleWolfMoves state)                   

getPossibleWolfMoves :: State -> [Position]
getPossibleWolfMoves state = filter (isFieldFreeWrapper state) [(Pos a b) | a <- [possibleX-1, possibleX+1], b <- [possibleY-1, possibleY+1]]
                              where possibleX = (x (wPosition state))
                                    possibleY = (y (wPosition state)) 

                                    
getPossibleWolfStates::State->[State]
getPossibleWolfStates state = (map (moveWolf state) (filter (isFieldFreeWrapper state) [(Pos a b) | a <- [possibleX-1, possibleX+1], b <- [possibleY-1, possibleY+1]]))
                              where possibleX = (x (wPosition state))
                                    possibleY = (y (wPosition state))                        

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

moveSheep :: State -> Position -> Position -> State
moveSheep state oldPos newPos = State (wPosition state) (sheepsPosition) 
                                    where sheepsPosition = moveSheepFromTo (sPosition state) oldPos newPos
 
moveSheepFromTo :: [Position] -> Position -> Position -> [Position]
moveSheepFromTo (s:sheeps) oldPos newPos = if s==oldPos then newPos : sheeps
                                                   else s : (moveSheepFromTo sheeps oldPos newPos)
                                          
isFieldFreeWrapper:: State->Position->Bool
isFieldFreeWrapper state pos = isFieldFree (wPosition state) (sPosition state) pos True

isFieldFree :: Position->[Position]-> Position -> Bool -> Bool                                      
isFieldFree wPos [] pos answer = if (x pos)>0 && (x pos)<9 && (y pos)>0 && (y pos)<9 
                                    && pos/=wPos && answer
                                    then True
                                    else False    
isFieldFree wPos (s:sheeps) pos answer = isFieldFree wPos sheeps pos (s/=pos && answer)                                    
                                                
getNewState :: State
getNewState = State (Pos 1 8) [Pos 2 1 ,Pos 4 1, Pos 6 1, Pos 8 1]     

moveWolf :: State -> Position -> State
moveWolf state newWPosition = State (newWPosition) (sPosition state)

-- Funkcja pomocnicza 
readInt :: String -> Int
readInt = read



 