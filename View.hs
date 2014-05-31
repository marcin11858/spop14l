module View where 

import Data.Char

import System.IO
import System.Directory

mainMenu = do
	putStrLn ("\nMENU GŁÓWNE")
	putStrLn ("[1] - Nowa gra")
	putStrLn ("[2] - Wczytaj gre")
	putStrLn ("[0] - Wyjdz")
	l <- getLine
	case (toNumber l) of
		1 -> do 
			 mainMenu
		0 -> do
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
	