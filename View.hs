module View where 

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
        'n':_ -> --do
                 --let newState = getNewState
                 
                 mainMenu
        'w':_ -> do
                 mainMenu
        'x':_ -> do
                 putStrLn ("Koniec")
        _ -> do
             putStrLn ("Niepoprawny wybor")
             mainMenu
    
toNumber line = 
    let 
        correctLength = length line > 0
        correctDigit = isDigit (line !! 0)
        digit = take 1 line
    in 
        if( correctLength && correctDigit)
            then read digit :: Int
            else -1
            

        
    