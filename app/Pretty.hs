module App.Pretty where 

import App.Type

-- how long i was searching in the prelude for this one... morto al Rusujo, nur ŝerce
class Show a => Pretty a where
    pretty :: a -> String
    --pretty = show

instance Pretty Term where
    pretty (Var (VarName name)) = name

    pretty (Comb cmbname []) = cmbname
    pretty (Comb cmbname combitems) = cmbname ++ tail
        where 
            tailmap = map (pretty) combitems
            tail = (foldl (++) "(" tailmap) ++ ")"
