module View where 

import Logic
import Data.Char

import System.IO
import System.Directory


mainMenu = do
    putStrLn ("\nMENU GŁÓWNE")
    putStrLn ("n - Nowa gra")
    putStrLn ("w - Wczytaj gre")
    putStrLn ("x - Wyjdz")
    cmd <- getLine
    case cmd of
        'n':_ -> do
                 let newState = getNewState
                 
                 gameMenu newState
        'w':_ -> do
                 mainMenu
        'x':_ -> do
                 putStrLn ("Koniec")
        _ -> do
             putStrLn ("Niepoprawny wybor")
             mainMenu
             
gameMenu state = do
    putStrLn ("\nMENU GRY")
    putStrLn ("z - Zapisz")
    putStrLn ("x - Powrót\n")
    printActualBoard state
    putStrLn ("")
    let wolfPossibleMoves = getPossibleWolfMoves state
    if (length wolfPossibleMoves) == 0 then do
                                           putStrLn ("Przegrales")
                                           mainMenu
    else do
        printPositions wolfPossibleMoves 1
        cmd <- getLine
        let choosenMove = toNumber cmd
        if choosenMove == -1 then case cmd of
                                       'z':[] -> do 
                                                 mainMenu
                                       'x':[] -> do 
                                                 mainMenu
                                       _      -> do 
                                                 putStrLn ("Niepoprawny wybór")
                                                 gameMenu state
                             else
                                makeMove choosenMove wolfPossibleMoves state
    
toNumber line = 
    let 
        correctLength = length line > 0
        correctDigit = isDigit (line !! 0)
        digit = take 1 line
    in 
        if( correctLength && correctDigit)
            then read digit :: Int
            else -1
            
makeMove::Int->[Position]->State->IO()
makeMove _ [] _ = do putStrLn ("wygrana")
makeMove n positions state = if n < 1  || n > (length positions) then do
                                                                    putStrLn("Niepoprawny ruch, sprobuj ponownie")
                                                                    gameMenu state
                                                                 else do
                                                                    let newPosition = (positions !! (n - 1))
                                                                    let stateAfterWolfMove = moveWolf state newPosition
                                                                    if y (wPosition stateAfterWolfMove) == 1 then do
                                                                                                                putStrLn ("Wygrales")
                                                                                                                mainMenu
                                                                                                             else
                                                                                                                gameMenu (findAndMakeSheepMove stateAfterWolfMove)
        
    