module Main where

--import System.IO (hFlush, hClose, hGetContents, stdout)
import System.IO
import System.Environment (getArgs)

data Todo = Todo {
    content :: String,
    completed :: Bool,
    priority :: Int
} deriving (Show)

type TodoList = Maybe [Todo]
type Input = String

-- Booting system
main :: IO TodoList
main = do
  contents <- readFile "todo.txt" 
  let todos = loadTodoList $ lines contents
  
  putStrLn "Welcome to my TODO List Manager!"
  loop todos

-- Main program loop
loop :: TodoList -> IO TodoList
loop todos = do
  storeTodoList todos
  putStr "todo> "
  hFlush stdout
  input <- getLine

  new <- handleInput todos input

  -- new will only be Nothing in the case
  -- of the "exit" command
  case new of
    Just n -> loop new
    Nothing -> return new

-- Based on the first value of "input",
-- decide what function to apply to the todo list
handleInput :: TodoList -> Input -> IO TodoList
handleInput todos ""      = help todos
handleInput todos input
  | command == "exit"     = exit
  | command == "help"     = help todos
  | command == "list"     = renderList todos
  | command == "ls"       = renderList todos
  | command == "add"      = add todos args
  | command == "complete" = complete todos input True
  | command == "uncheck"  = complete todos input False
  | command == "edit"     = edit todos input 
  | command == "delete"   = delete todos input
  | command == "rm"       = delete todos input
  | otherwise = noop todos command
  where command = head $ words input
        args    = unwords $ tail $ words input

exit :: IO TodoList
exit = do
  putStrLn "Goodbye!"
  return Nothing

help :: TodoList -> IO TodoList
help todos = do
  putStrLn "available commands: \n\
\ help \n\
\ list (ls) <filter, optional> \n\
\ add <description> \n\
\ complete <index> \n\
\ uncheck <index> \n\
\ edit <index> <new description> \n\
\ delete (rm) <index> \n\
\ exit"
  return todos

add :: TodoList -> Input -> IO TodoList
add (Just todos) "" = do return $ Just todos
add (Just todos) input = do
  let new_todos = todos ++ [(Todo input False 1)]
  return $ Just new_todos

noop :: TodoList -> Input -> IO TodoList
noop todos command = do
  putStrLn $ "Unrecognized command: " ++ command
  return todos

complete :: TodoList -> Input -> Bool -> IO TodoList
complete todos ""  _         = return $ todos
complete (Just []) input _   = return $ Just []
complete (Just todos) input bool
  | index < 0   = do 
        putStrLn "Index is out-of-bounds"
        return $ Just todos
  | index > (length todos) = do 
        putStrLn "Index is out-of-bounds"
        return $ Just todos
  | otherwise = do return $ todoCompleted (Just todos) index bool
  where index = read $ (head . tail $ words input) :: Int

edit :: TodoList -> Input -> IO TodoList
edit todos ""          = return $ todos
edit (Just []) input   = return $ Just []
edit (Just todos) input 
  | index < 0   = do 
        putStrLn "Index is out-of-bounds"
        return $ Just todos
  | index > (length todos) = do 
        putStrLn "Index is out-of-bounds"
        return $ Just todos
  | otherwise = do return $ todoEdited (Just todos) index desc
  where index = read $ (head . tail $ words input) :: Int
        desc = unwords (tail . tail $ words input)

delete :: TodoList -> Input -> IO TodoList
delete todos ""           = return $ todos
delete (Just []) input    = return $ Just []
delete (Just todos) input 
  | index < 0   = do 
        putStrLn "Index is out-of-bounds"
        return $ Just todos
  | index > (length todos) = do 
        putStrLn "Index is out-of-bounds"
        return $ Just todos
  | otherwise = do return $ todoDeleted (Just todos) index
  where index = read $ (head . tail $ words input) :: Int

todoDeleted :: TodoList -> Int -> TodoList
todoDeleted (Just todos) index = do
  let (x1, todo:x2) = splitAt (index - 1) todos 
  Just $ (x1 ++ x2)

todoCompleted :: TodoList -> Int -> Bool -> TodoList
todoCompleted (Just todos) index is_completed = do
  let (x1, todo:x2) = splitAt (index - 1) todos 
  Just $ (x1 ++ [todo { completed = is_completed } ] ++ x2)

todoEdited :: TodoList -> Int -> Input -> TodoList
todoEdited (Just todos) index new_description = do
  let (x1, todo:x2) = splitAt (index - 1) todos 
  Just $ (x1 ++ [todo { content = new_description } ] ++ x2)

-- Rendering the to-do list
renderList :: TodoList -> IO TodoList
renderList (Just todos) = do
  putStrLn "## To-Do List ##" 
  mapM_ (putStrLn . renderTodo) $ zip todos [1..]
  putStrLn "\n"
  return $ Just todos

renderTodo :: (Todo,Int) -> String
renderTodo (todo, index) = (show index) ++ (checkbox todo) ++ (content todo)

checkbox :: Todo -> String
checkbox todo
  | completed todo = " [x] "
  | otherwise = " [ ] "

-- Persistence
-- Recursive Load
loadTodoList :: [String] -> TodoList
loadTodoList [] = Just []
loadTodoList (x:[]) = Just [loadTodo x]
loadTodoList (x:xs) = (:) <$> (Just (loadTodo x)) <*> (loadTodoList xs) 
-- The above syntax still confuses me honestly, <$> and <*> ??

loadTodo :: String -> Todo
loadTodo string = (Todo content completed priority)
  where x:y:z = words string
        completed = (read x) :: Bool -- First word
        priority = (read y)  :: Int  -- Second word
        content = unwords z   -- Third and onwards

storeTodoList :: TodoList -> IO ()
storeTodoList (Nothing) = return ()
storeTodoList (Just todos) = writeFile "todo.txt" (unlines $ fmap (dumpTodo) todos)

dumpTodo :: Todo -> String
dumpTodo todo = unwords [(show $ completed todo), (show $ priority todo), content todo]
