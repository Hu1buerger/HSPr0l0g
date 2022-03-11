module App.CommandLine where

import Data.Either

import App.Pretty
import App.Parser (simpleParse, goal, parseFile)
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
        else       
            putStrLn ("error: " ++ fromLeft "" res)

type State = (Either String (Strategy, Prog, Maybe FilePath))

eval :: Strategy -> Prog ->  (Maybe FilePath) -> String -> IO (Either String (Strategy, Prog, Maybe FilePath))
eval s p f (':':c:ss)
    | c == 'h' = undefined 
    | c == 'l' = loadProg
    | c == 'p' = printProg
    | c == 'q' = undefined
    | c == 'r' = reload f
    | c == 's' = setStrategy ss
    | c == 't' = undefined
    | otherwise = fail "no catch 22"
        where 
            -- helper 
            fail error = return $ Left error 
            success st pr lf = return (Right (st, pr, lf))

            -- l
            loadProg = do 
                putStrLn "Start loading:"
                let fileName = tail ss
                putStrLn $ "filename: " ++ fileName
                loadRes <- parseProg fileName -- :: Either String Prog
                if isLeft loadRes
                    then fail (fromLeft "" loadRes)
                    else do 
                        let (Right prog) = loadRes
                        success s prog (Just fileName)

            -- h

            -- p
            printProg = do 
                putStrLn "Loaded Program:"
                putStrLn (pretty p)
                success s p f

            -- q

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
            setStrategy " bfs" = printDes "bfs" bfs
            setStrategy " dfs" = printDes "dfs" dfs
            setStrategy arg = fail $ "illegal " ++ arg

            printDes name strat = do 
                putStrLn $ "Stategy set to " ++ name
                success strat p f

            -- t
eval _ _ _ _ = return $ Left "no catch"


parseProg :: FilePath -> IO (Either String Prog)
parseProg = parseFile