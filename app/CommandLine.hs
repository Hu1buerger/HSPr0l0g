module App.CommandLine where

import Prelude hiding (fail)

import Data.Either

import App.Pretty
import App.Parser (parseFile, parse)
import App.SLDResolution
import App.Type

readCommand :: IO String
readCommand = do 
    putStr "?- "
    getLine

startRepl :: IO ()
startRepl = repl dfs (Prog []) Nothing

repl :: Strategy -> Prog -> (Maybe FilePath) -> IO ()
repl strat prog filepath = do 
    cmd <- readCommand
    putStr "\n"
    res <- eval strat prog filepath cmd
    let suc = isRight res
    if suc 
        then do 
            let (Right (ns, ps, lf)) = res
            repl ns ps lf
        else do      
            putStrLn ("error: " ++ fromLeft "" res)
            repl strat prog filepath

type State = (Either String (Strategy, Prog, Maybe FilePath))

eval :: Strategy -> Prog ->  (Maybe FilePath) -> String -> IO (Either String (Strategy, Prog, Maybe FilePath))

-- helper 
fail :: String -> IO (State)
fail errormsg = return $ Left errormsg 

success :: Strategy -> Prog ->  (Maybe FilePath) -> IO (State)
success st pr lf = return (Right (st, pr, lf))

-- commands
eval s p f (':':c:ss)
    | c == 'h' = helpIAmDisabled 
    | c == 'l' = loadProg
    | c == 'p' = printProg
    | c == 'q' = quit
    | c == 'r' = reload f
    | c == 's' = setStrategy $ tail ss
    | c == 't' = sldTree $ tail ss
    | otherwise = fail "invalid command"
        where 
            -- l
            loadProg = do 
                putStrLn "Start loading:"
                let fileName = ss
                putStrLn $ "filename: " ++ fileName
                loadRes <- parseProg fileName -- :: Either String Prog
                if isLeft loadRes
                    then fail (fromLeft "" loadRes)
                    else do 
                        let (Right prog) = loadRes
                        success s prog (Just fileName)

            -- h
            helpIAmDisabled =
                do 
                    putStrLn "Commands available from the prompt:"
                    putStrLn "    <goal>        Solves/proves the specified goal."
                    putStrLn "    :h            Shows this help message."
                    putStrLn "    :l <file>     Loads the specified file."
                    putStrLn "    :p            Prints the currently loaded program."
                    putStrLn "    :q            Exits the interactive environment."
                    putStrLn "    :r            Reloads the last loaded file."
                    putStrLn "    :s <strat>    Sets the specified search strategy"
                    putStrLn "                      where <strat> is one of 'dfs', 'bfs'."
                    putStrLn "    :t <goal>     Prints the SLD tree for the specified goal."
                    success s p f
            -- p
            printProg = do 
                putStrLn "Loaded Program:"
                putStrLn (pretty p)
                success s p f

            -- q
            quit = error "bye..."

            -- r
            reload :: Maybe FilePath -> IO (State)
            reload (Nothing) = fail "cannot reload, no last loaded file"
            reload (Just filepath) = do
                putStrLn ("Reloading " ++ filepath)
                loadRes <- parseProg filepath
                if isLeft loadRes 
                    then fail ("file missing \n \t" ++ (fromLeft "def" loadRes))
                    else success s (fromRight (Prog []) loadRes) (Just filepath)

            -- s 
            setStrategy "bfs" = printDes "bfs" bfs
            setStrategy "dfs" = printDes "dfs" dfs
            setStrategy arg = fail $ "illegal " ++ arg

            printDes name strat = do 
                putStrLn $ "Stategy set to " ++ name
                success strat p f

            -- t
            sldTree query = 
                do 
                    let goalM = parseGoal query
                    _ <- if isLeft goalM 
                        then fail (fromLeft (error "nope") goalM)
                        else do
                            let goal' = fromRight (error "nope") goalM
                            let tree = sld p goal'
                            let prettyTree = pretty tree
                            putStr prettyTree
                            success s p f
                    success s p f
eval s p f input 
    | head input == ':' = success s p f
    | isGoal = 
        do  
            --putStrLn ( "asked: " ++ pretty goal)
            let sol = solveWith p goal' s
            if sol == [] 
                then do
                    putStrLn "No solution"
                else do
                    putStrLn (concatMap pretty sol)
                    putStrLn ""
            success s p f 
    | otherwise = fail errormessage
    where 
        maybeGoal = parseGoal input
        isGoal = isRight maybeGoal
        goal' = fromRight (error "default") maybeGoal
        errormessage = fromLeft (error "default") maybeGoal


parseProg :: FilePath -> IO (Either String Prog)
parseProg = parseFile

parseGoal :: String -> Either String Goal 
parseGoal = parse