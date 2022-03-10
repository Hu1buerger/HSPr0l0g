module App.CommandLine where

import App.Parser (simpleParse, goal)
import App.SLDResolution
import App.Type

readGoal = do
    line <- getLine
    return (simpleParse goal line)

readCommand :: IO String
readCommand = do 
    putStr "?- "
    getLine

startRepl :: IO ()
startRepl = repl dfs (Prog [])

repl :: Strategy -> Prog -> IO ()
repl strat prog = do 
    cmd <- readCommand
    putStr "\n"
    res <- eval strat prog cmd
    putStrLn (show res)


eval :: Strategy -> Prog ->  String -> IO (Maybe (Strategy, Prog))
eval s p (':':c:' ':ss)
    | c == 'h' = undefined 
    | c == 'l' = undefined
    | c == 'p' = undefined
    | c == 'q' = undefined
    | c == 'r' = undefined
    | c == 's' = setStrategy ss
    | c == 't' = undefined
    | otherwise = return Nothing
        where 
            setStrategy "bfs" = printDes "bfs" bfs
            setStrategy "dfs" = printDes "dfs" dfs
            setStrategy _ = return Nothing

            printDes name strat = do 
                putStrLn $ "Stategy set to " ++ name
                return (Just (strat, p))
