module App.CommandLine where

import App.Parser (simpleParse, goal)

readGoal = do
    line <- getLine
    return (simpleParse goal line)