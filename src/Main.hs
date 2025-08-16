module Main where

import System.IO (hFlush, stdout)
import System.Console.ANSI

data Mark = X | O | None deriving (Eq) 

instance Show Mark where
  show X = "X"
  show O = "O"
  show None = " "

data Row = Row Mark Mark Mark
data Board = Board Row Row Row Mark

main :: IO ()
main = do
  putStrLn "Welcome to Tic Tac Toe!"
  board <-  mkBoard
  loop board

mkBoard :: IO Board
mkBoard = pure $ (Board 
  (Row None None None) 
  (Row None None None) 
  (Row None None None)
  X)

loop :: Board -> IO ()
loop board = do
  clearScreen
  render board
  putStr "Enter command: "
  hFlush stdout
  input <- getLine
  updatedBoard <- handleInput input board
  case (gameOver updatedBoard) of
    Just mark -> declareWinner updatedBoard mark
    Nothing   -> loop updatedBoard

declareWinner :: Board -> Mark -> IO ()
declareWinner (Board r1 r2 r3 _) X = do
  clearScreen
  putStrLn "\n"
  printRow r1
  spacer
  printRow r2
  spacer
  printRow r3
  putStrLn "\n"
  putStrLn "X wins"
  putStrLn "\n"

declareWinner (Board r1 r2 r3 _) O = do
  clearScreen
  putStrLn "\n"
  printRow r1
  spacer
  printRow r2
  spacer
  printRow r3
  putStrLn "\n"
  putStrLn "O wins"
  putStrLn "\n"

declareWinner board None = do
  clearScreen
  render board
  putStrLn "cat's game"

handleInput :: String -> Board -> IO Board
handleInput "exit" (Board r1 r2 r3 _) = do
  putStrLn "Goodbye!"
  pure $ (Board r1 r2 r3 None)

handleInput "q" (Board (Row None w e) r2 r3 mark) = do
  pure $ (Board (Row mark w e) r2 r3 (swap mark))

handleInput "w" (Board (Row q None e) r2 r3 mark) = do
  pure $ (Board (Row q mark e) r2 r3 (swap mark))

handleInput "e" (Board (Row q w None) r2 r3 mark) = do
  pure $ (Board (Row q w mark) r2 r3 (swap mark))

handleInput "a" (Board r1 (Row None s d) r3 mark) = do
  pure $ (Board r1 (Row mark s d) r3 (swap mark))

handleInput "s" (Board r1 (Row a None d) r3 mark) = do
  pure $ (Board r1 (Row a mark d) r3 (swap mark))

handleInput "d" (Board r1 (Row a s None) r3 mark) = do
  pure $ (Board r1 (Row a s mark) r3 (swap mark))

handleInput "z" (Board r1 r2 (Row None x c) mark) = do
  pure $ (Board r1 r2 (Row mark x c) (swap mark))

handleInput "x" (Board r1 r2 (Row z None c) mark) = do
  pure $ (Board r1 r2 (Row z mark c) (swap mark))

handleInput "c" (Board r1 r2 (Row z x None) mark) = do
  pure $ (Board r1 r2 (Row z x mark) (swap mark))

handleInput "reset" _ = mkBoard

handleInput "help" board = do
  putStrLn "Tic-Tac-Toe!"
  putStrLn "available commands:"
  putStrLn "q w e"
  putStrLn "a s d - Place a mark on the board"
  putStrLn "z x c"
  putStrLn ""
  putStrLn "reset - Reset the game"
  putStrLn "help  - Display this message"
  pure $ board

handleInput input board = do
  putStrLn $ "Invalid command: '" ++ input ++ "'"
  pure board

swap :: Mark -> Mark
swap X = O
swap O = X

match :: Mark -> Mark -> Mark -> Bool
match m1 m2 m3 = (m1 /= None) && (m1 == m2 && m2 == m3)

gameOver :: Board -> Maybe Mark
gameOver (Board (Row q w e) (Row a s d) (Row z x c) mark)
  | (mark == None) = Just None
  | match q w e = Just q  
  | match a s d = Just a
  | match z x c = Just z
  | match q a z = Just q
  | match w s x = Just w
  | match e d c = Just e
  | match q s c = Just q
  | match e s z = Just e
  | all (\m -> m /= None) [q, w, e, a, s, d, z, x, c] = Just None
  | otherwise = Nothing

render :: Board -> IO ()
render (Board r1 r2 r3 mark) = do
  putStrLn "\n"
  printRow r1
  spacer
  printRow r2
  spacer
  printRow r3
  putStrLn "\n"
  putStr $ "now playing: "
  setSGR [SetConsoleIntensity BoldIntensity]
  setSGR [SetColor Foreground Vivid Blue]
  putStr $ show mark
  setSGR [Reset]
  putStrLn ""
  putStrLn "\n"

printRow :: Row -> IO ()
printRow (Row m1 m2 m3) = do
  putStrLn $ (show m1) ++ " | " ++ (show m2) ++ " | " ++ (show m3)

spacer :: IO ()
spacer = putStrLn "__|___|__"
