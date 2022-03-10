module App.SubstType where

import App.Type

data Subst = Subst [(VarName, Term)]
    deriving (Eq, Show)
