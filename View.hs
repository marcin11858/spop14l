--SPOP 14L, Wilk i owce, Adam Prus, Marcin Wlazły 

-- Moduł odpowiadający za widok - wyświetlanie gry
module View where 

import Logic
import Data.Char

import System.IO
import System.Directory

--Funkcja startująca grę
startGame = do
            printLogo
            mainMenu

-- Funkcja drukuje Menu i czeka na reakcję użytkownika
mainMenu = do
    printMainMenu
    cmd <- getLine
    case cmd of
        'n':_ -> do
                 let newState = getNewState
                 gameMenu newState
        'w':_ -> do
                 readFromFile
        'x':_ -> do
                 printEnd
        _ -> do
             printWrongOption
             mainMenu
     
 -- Funkcja zawierająca pętlę gry
gameMenu state = do
    printGameMenu
    printActualBoard state
    putStrLn ("")
    let wolfPossibleMoves = getPossibleWolfMoves state
    if (length wolfPossibleMoves) == 0 then do
                                           printLose
                                           mainMenu
    else do
        printMovesTitle
        printPositions wolfPossibleMoves 1
        cmd <- getLine
        let choosenMove = toNumber cmd
        if choosenMove == -1 then case cmd of
                                       'z':[] -> do
                                                 saveToFile state
                                                 gameMenu state
                                       'x':[] -> do 
                                                 mainMenu
                                       _      -> do 
                                                 printWrongOption
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
makeMove _ [] _ = do printWin
makeMove n positions state = if n < 1  || n > (length positions) then do
                                                                    printWrongOption
                                                                    gameMenu state
                                                                 else do
                                                                    let newPosition = (positions !! (n - 1))
                                                                    let stateAfterWolfMove = moveWolf state newPosition
                                                                    if y (wPosition stateAfterWolfMove) == 1 then do
                                                                                                                printWin
                                                                                                                mainMenu
                                                                                                             else
                                                                                                                gameMenu (findAndMakeSheepMove stateAfterWolfMove)
 -- Funkcja zwraca czystą planszę gry
getClearBoard :: [[String]]
getClearBoard =   [[" ", "1", "2", "3", "4", "5", "6", "7", "8"],
                   ["1", ".", " ", ".", " ", ".", " ", ".", " "],
                   ["2", " ", ".", " ", ".", " ", ".", " ", "."],
                   ["3", ".", " ", ".", " ", ".", " ", ".", " "],
                   ["4", " ", ".", " ", ".", " ", ".", " ", "."],
                   ["5", ".", " ", ".", " ", ".", " ", ".", " "],
                   ["6", " ", ".", " ", ".", " ", ".", " ", "."],
                   ["7", ".", " ", ".", " ", ".", " ", ".", " "],
                   ["8", " ", ".", " ", ".", " ", ".", " ", "."]]
      
 -- Funkcja drukuje planszę z postaci tablicy Stringów  
printBoard :: [[String]] -> IO ()
printBoard str = do
            printBoardTitle
            mapM_ putStrLn [ b | b <- [unwords list_str | list_str <- str]]
      
 -- Funkcja drukuje planszę na podstwaie stanu gry- główna funkcja wyświetlająca stan planszy
printActualBoard ::State -> IO ()
printActualBoard state = printBoard (setBoard (wPosition state) (sPosition state) getClearBoard)  

 -- Funkcja ustawia pozycje na planszy
setBoard::Position->[Position]->[[String]]->[[String]]
setBoard pos [] tab = updateMatrix tab "W" (y pos, x pos)
setBoard pos (z:zs) tab = setBoard pos zs (updateMatrix tab "O" (y z, x z))

 -- Funkcja ustawia pozycje na planszy
printPositions::[Position]->Int-> IO () 
printPositions [] _ = putStr ""                  
printPositions (p:pos) n  = do  putStr "["
                                putStr ( show n)
                                putStr "] -> ("
                                putStr (show(x p)) 
                                putStr ","     
                                putStr (show(y p)) 
                                putStrLn ")"
                                printPositions pos (n+1)

 -- Funkcja uaktualniająca dwuwyamirową macierz o element na pozycji (y,x)                             
updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix m x (r,c) =  take r m ++  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++ drop (r + 1) m                 
    
 -- Wczytwanie stanu gry z pliku  
readFromFile = do
    printPutFilename
    fileName <- getLine
    fileExists <- doesFileExist fileName
    readPositions fileExists fileName

readSheepsPositions::[Int]->[Position]
readSheepsPositions [] = []
readSheepsPositions (x:y:positions) = (Pos x y) : (readSheepsPositions positions)
    

readPositions False _ = do
                        printWrongFilename
                        mainMenu  


readPositions True fileName = do
                              content <- readFile fileName
                              let xTmp:yTmp:positions = map readInt . words $ content  
                              let wPos = Pos xTmp yTmp
                              let sPos = readSheepsPositions positions
                              let state = State wPos sPos
                              gameMenu state
-- Zapis stanu do pliku
saveToFile state = do
    printPutFilename
    fileName <- getLine
    handle <- openFile fileName WriteMode
    savePositions ((wPosition state) : (sPosition state)) handle
    hClose handle
-- Zapis do pliku jest realizowany przez zapis współrzędnych wilka, a później linia po linii owiec
savePositions []  handle = do
                            return()
savePositions (z:zs)  handle = do
                hPutStr handle (show (x z))
                hPutStr handle (" ")
                hPutStrLn handle (show (y z))
                savePositions zs  handle
-- Komunikaty                              
printLogo = do 
    putStrLn "         _                                                      _,._    "
    putStrLn "        / \\      _-'                                       __.'   _)   "
    putStrLn "      _/|  \\-''- _ /                                      <_,)'.-\"a\\ "
    putStrLn " __-' { |          \\             WILK                       /' (    \\ "
    putStrLn "     /              \\                           _.-----..,-'   (`\"--^ "
    putStrLn "     /       \"o.  |o }             I           //              |       "
    putStrLn "     |            \\ ;                         (|   `;      ,   |       "
    putStrLn "                   ',            OWCE            \\   ;.----/  ,/       "
    putStrLn "        \\_         __\\                          ) // /   | |\\ \\     "
    putStrLn "          ''-_    \\.//                           \\ \\\\`\\   | |/ /   "
    putStrLn "            / '-____'                              \\ \\\\ \\  | |\\/   "
    putStrLn "           /                                        `\" `\"  `\"`       "
    putStrLn "                                       Autorzy: Adam Prus, Marcin Wlazły"

printMainMenu = do 
    putStrLn ("|------------------------------------------------------------------------|")
    putStrLn ("|                            MENU GŁÓWNE                                 |")
    putStrLn ("|                                                                        |")
    putStrLn ("|                            n - Nowa gra                                |")
    putStrLn ("|                          w - Wczytaj gre                               |")
    putStrLn ("|                             x - Wyjdź                                  |")
    putStrLn ("|------------------------------------------------------------------------|")

printGameMenu = do
    putStrLn ("|------------------------------------------------------------------------|")
    putStrLn ("|                               MENU GRY                                 |")
    putStrLn ("|                                                                        |")
    putStrLn ("|                               z - Zapisz                               |")
    putStrLn ("|                               x - Powrót                               |")
    putStrLn ("|                          1..4 - Wybrany ruch                           |")
    putStrLn ("|------------------------------------------------------------------------|")

printEnd = do
    putStrLn ("|------------------------------------------------------------------------|")
    putStrLn ("|                                                                        |")
    putStrLn ("|                              DO WIDZENIA                               |")
    putStrLn ("|                                                                        |")
    putStrLn ("|------------------------------------------------------------------------|")

printWrongOption = do
    putStrLn ("|------------------------------------------------------------------------|")
    putStrLn ("|                                                                        |")
    putStrLn ("|                 NIEPOPRAWNY WYBÓR, SPRÓBUJ PONOWNIE                    |")
    putStrLn ("|                                                                        |")
    putStrLn ("|------------------------------------------------------------------------|")

printLose = do
    putStrLn ("|------------------------------------------------------------------------|")
    putStrLn ("|                                                                        |")
    putStrLn ("|                             PRZEGRAŁEŚ                                 |")
    putStrLn ("|                                                                        |")
    putStrLn ("|------------------------------------------------------------------------|")

printWin = do
    putStrLn ("|------------------------------------------------------------------------|")
    putStrLn ("|                                                                        |")
    putStrLn ("|               !!!            WYGRAŁEŚ            !!!                   |")
    putStrLn ("|                                                                        |")
    putStrLn ("|------------------------------------------------------------------------|")
    
printPutFilename = do
    putStrLn ("|------------------------------------------------------------------------|")
    putStrLn ("|                                                                        |")
    putStrLn ("|                         PODAJ NAZWĘ PLIKU:                             |")
    putStrLn ("|                                                                        |")
    putStrLn ("|------------------------------------------------------------------------|")
    
printWrongFilename = do
    putStrLn ("|------------------------------------------------------------------------|")
    putStrLn ("|                                                                        |")
    putStrLn ("|                      NIEPOPRAWNA NAZWA PLIKU                           |")
    putStrLn ("|                                                                        |")
    putStrLn ("|------------------------------------------------------------------------|")
    
printBoardTitle  = do
    putStrLn ("|------------------------------------------------------------------------|")
    putStrLn ("|                       AKTUALNY STAN PLANSZY                            |")
    putStrLn ("|------------------------------------------------------------------------|")

printMovesTitle  = do
    putStrLn ("|------------------------------------------------------------------------|")
    putStrLn ("|                          DOSTĘPNE RUCHY                                |")
    putStrLn ("|------------------------------------------------------------------------|")