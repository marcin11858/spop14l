--SPOP 14L, Wilk i owce, Adam Prus, Marcin Wlazły 

-- Logika gry
module Logic  where

import Control.Monad 
import Data.List
import Data.Ord
import Text.Read
import System.IO
import System.Directory

-- Typ reprezentujący pozycję na planszy jako współrzędne x i y
data Position = Pos {x::Int, y::Int} deriving (Show)

-- Typ reprezentujący węzeł drzewa przeszukiwań, skłąda się z 3 elementów:
--  [1] - stan związany z danym wezłęm 
--  [2] - lista węzłów podpiętych pod dany węzeł
--  [3]-  wartość funkcji oceniającej
data Tree = Tree {state::State, tree::[Tree], funValue::Int} deriving (Show)

--Operator porównywania da typu Position
instance Eq Position where
                (Pos x1 y1) == (Pos x2 y2) = (x1 == x2) &&  (y1 == y2) 
                
-- Typ reprezentujący stan gry, zawiera:
--  [1] - pozycję wilka
--  [2] - lista pozycji owiec
data State = State { wPosition::Position,
                     sPosition::[Position]} deriving (Show)

 -- Funkcja zwracjąca ruch owcy, tworzy drzewo i algorytmem minmax znajduje najlepszy ruch. Tu również podaje się poziom drzewa.                 
findAndMakeSheepMove::State->State
findAndMakeSheepMove state = do
                             let tree = createTree state 5
                             snd (minmax 0 tree)

-- Tworzenie drzewa: główna fukcnja, która wywołuje funkcję rekurencyjną, przyjmuje stan wierzchołka korzenia oraz jego poziom zagłębienia
-- [1] - stan początkowy
-- [2] - liczba poziomów drzewa
createTree::State->Int->Tree
createTree state depth = createTreeLevel depth 0 state 

-- Tworzenie drzewa: funkcja rekurencyjna, która operuje na kolejych poziomach drzewa
createTreeLevel::Int->Int->State->Tree -- drzewo rodzic, glebokosc drzewa, 
createTreeLevel 0 _ state = Tree  state [] (evaluateState state)
createTreeLevel depth position state =  Tree  state (map (createTreeLevel (depth-1) (position+1)) (getNextStates state position)) (-1)


-- Algorytm minmax: Zwraca wartość funkcji oceny oraz stan
-- W wywłoaniu początkowym należy podać 0 oraz drzewo do przeszukania
minmax::Int->Tree->(Int, State)
minmax _ (Tree  state [] fValue) = ((evaluateState state), state)
minmax 0 (Tree  state tree fValue) =  getMaximumTuple (map (minmax (1)) tree)                                                        
minmax depth (Tree  state tree fValue) = if mod depth 2 == 0 then (fst (getMaximumTuple (map (minmax (depth + 1)) tree)), state)
                                                              else (fst (getMinimumTuple (map (minmax (depth + 1)) tree)), state)

-- Funkcja zwracająca parę (ocena, stan) o największej  wartości oceny z listy par (ocena, stan)                                                    
getMaximumTuple::[(Int, State)]->(Int, State)
getMaximumTuple list = maximumBy (comparing fst) list

-- Funkcja zwracająca parę (ocena, stan) o najmniejszej wartości oceny z listy par (ocena, stan)   
getMinimumTuple::[(Int, State)]->(Int, State)
getMinimumTuple list = minimumBy (comparing fst) list

-- Funkcja zwracająca możliwe stany gry na podstawie zadanego stanu oraz levelu, który determinuje czy ruch należy do owiec czy do wilka
getNextStates::State->Int->[State]
getNextStates state level = if mod level 2 == 0 then getPossibleSheepsStates (sPosition state) state
                                                      else getPossibleWolfStates state 
-- Funkcja oceny stanu dla drzewa
evaluateState::State->Int
evaluateState state =  alpha1 * (sheepDistribution (sPosition state)) + alpha2 * (wolfNeighborhood (wPosition state) (sPosition state)) + alpha3 * ( y (wPosition state)) + alpha4 * (getCountPossibleMoves state)
                        where alpha1 = -4
                              alpha2 = -1
                              alpha3 = 4
                              alpha4 = 4
                              
-- Kryterium 1 - rozproszenie owiec w rzędzie
sheepDistribution::[Position]->Int
sheepDistribution positions = (maxSheepY positions) - (minSheepY positions)

-- Funkcja pomocnicza - maksymalna współrzędna Y ze wszystkich owiec
maxSheepY::[Position]->Int
maxSheepY [] = 0
maxSheepY (z:zs) = max (y z) (maxSheepY zs)

-- Funkcja pomocnicza - minimalna współrzędna Y ze wszystkich owiec
minSheepY::[Position]->Int
minSheepY [] = 9999999999999
minSheepY (z:zs) = min (y z) (minSheepY zs) 

-- Kryterium 2 - sasiedztwo wilka (suma odległości od owiec (manhattan))
wolfNeighborhood::Position->[Position]->Int
wolfNeighborhood _ [] = 0
wolfNeighborhood wolfPos (z:zs) = (abs ((x wolfPos) - ( x z) )) + (abs ((y wolfPos) - ( y z) )) + (wolfNeighborhood wolfPos zs) 

-- Kryterium 4 - Liczba możliwych ruchów wilka
getCountPossibleMoves::State->Int
getCountPossibleMoves state = if length (getPossibleWolfMoves state) ==  0 then 99 else 4 -  length (getPossibleWolfMoves state)                   

-- Możliwe ruchy wilka na podstawie jego obecnego stanu zwraca listę pozycji
getPossibleWolfMoves :: State -> [Position]
getPossibleWolfMoves state = filter (isFieldFreeWrapper state) [(Pos a b) | a <- [possibleX-1, possibleX+1], b <- [possibleY-1, possibleY+1]]
                              where possibleX = (x (wPosition state))
                                    possibleY = (y (wPosition state)) 

-- Możliwe ruchy wilka na podstawie jego obecnego stanu zwraca listę stanów gry                                    
getPossibleWolfStates::State->[State]
getPossibleWolfStates state = (map (moveWolf state) (filter (isFieldFreeWrapper state) [(Pos a b) | a <- [possibleX-1, possibleX+1], b <- [possibleY-1, possibleY+1]]))
                              where possibleX = (x (wPosition state))
                                    possibleY = (y (wPosition state))                        
-- Możliwe ruchy owiec
getPossibleSheepMoves :: [Position] -> State -> [Position]
getPossibleSheepMoves [] _ = []
getPossibleSheepMoves (z:zs) state = (filter (isFieldFreeWrapper state)[(Pos a possibleY) | a <- [possibleX-1, possibleX+1]]) ++ (getPossibleSheepMoves zs state) 
                                     where possibleX = (x z)
                                           possibleY = (y z) + 1

-- Możliwe stany planyszy po ruchu owiec
getPossibleSheepsStates::[Position]->State->[State]
getPossibleSheepsStates [] _ = []
getPossibleSheepsStates (z:zs) state = (map (moveSheep state z) (filter (isFieldFreeWrapper state)[(Pos a ((y z) + 1)) | a <- [(x z)-1, (x z)+1]]))
                                            ++ (getPossibleSheepsStates zs state)
-- Ruch owcy: stan, stara pozycja, nowa pozycja - zwraca stan, w którym owca stoi na nowej pozycji
moveSheep :: State -> Position -> Position -> State
moveSheep state oldPos newPos = State (wPosition state) (sheepsPosition) 
                                    where sheepsPosition = moveSheepFromTo (sPosition state) oldPos newPos
 
 -- Funkcja przenosi owcę ze starej pozcyji na nową, zwraca listę pozycji owiec
moveSheepFromTo :: [Position] -> Position -> Position -> [Position]
moveSheepFromTo (s:sheeps) oldPos newPos = if s==oldPos then newPos : sheeps
                                                   else s : (moveSheepFromTo sheeps oldPos newPos)
-- Zwraca czy pole jest wolne - funkcja opakowująca                                      
isFieldFreeWrapper:: State->Position->Bool
isFieldFreeWrapper state pos = isFieldFree (wPosition state) (sPosition state) pos True

-- Zwraca czy pole jest wolne - funkcja rekurenyjna
isFieldFree :: Position->[Position]-> Position -> Bool -> Bool                                      
isFieldFree wPos [] pos answer = if (x pos)>0 && (x pos)<9 && (y pos)>0 && (y pos)<9 
                                    && pos/=wPos && answer
                                    then True
                                    else False    
isFieldFree wPos (s:sheeps) pos answer = isFieldFree wPos sheeps pos (s/=pos && answer)                                    
      
-- Zwraca początkowy stan gry      
getNewState :: State
getNewState = State (Pos 1 8) [Pos 2 1 ,Pos 4 1, Pos 6 1, Pos 8 1]     

-- Ruch wilka na podstawie obecengo stanu i nowej pozycji zwraca nowy stan gry, gdzie wilk stoi na nowej pozycji
moveWolf :: State -> Position -> State
moveWolf state newWPosition = State (newWPosition) (sPosition state)

-- Funkcja pomocnicza 
readInt :: String -> Int
readInt = read



 