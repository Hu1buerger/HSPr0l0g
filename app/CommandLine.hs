module CommandLine where

import Parser (simpleParse, goal)

readGoal = do
    line <- getLine
    return (simpleParse goal line)