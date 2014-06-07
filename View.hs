module View where 

import Logic
import Data.Char

import System.IO
import System.Directory

startGame = do
            printLogo
            mainMenu

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
             
gameMenu state = do
    printGameMenu
    printActualBoard state
    putStrLn ("")
    let wolfPossibleMoves = getPossibleWolfMoves state
    if (length wolfPossibleMoves) == 0 then do
                                           printLose
                                           mainMenu
    else do
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

readFromFile = do
    printPutFilename
    fileName <- getLine
    fileExists <- doesFileExist fileName
    readPositions fileExists fileName

readSheepsPositions::[Int]->[Position]
readSheepsPositions [] = []
readSheepsPositions (x:y:positions) = (Pos x y) : (readSheepsPositions positions)
    
readPositions False _ = mainMenu  


readPositions True fileName = do
                              content <- readFile fileName
                              let xTmp:yTmp:positions = map readInt . words $ content  
                              let wPos = Pos xTmp yTmp
                              let sPos = readSheepsPositions positions
                              let state = State wPos sPos
                              gameMenu state
                              
printLogo = do 
    putStrLn "         _                                                _,._    "
    putStrLn "        / \\      _-'                                 __.'   _)   "
    putStrLn "      _/|  \\-''- _ /                                <_,)'.-\"a\\ "
    putStrLn " __-' { |          \\          WILK                    /' (    \\ "
    putStrLn "     /              \\                     _.-----..,-'   (`\"--^ "
    putStrLn "     /       \"o.  |o }          I        //              |       "
    putStrLn "     |            \\ ;                   (|   `;      ,   |       "
    putStrLn "                   ',         OWCE         \\   ;.----/  ,/       "
    putStrLn "        \\_         __\\                    ) // /   | |\\ \\     "
    putStrLn "          ''-_    \\.//                     \\ \\\\`\\   | |/ /   "
    putStrLn "            / '-____'                        \\ \\\\ \\  | |\\/   "
    putStrLn "           /                                  `\" `\"  `\"`       "

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