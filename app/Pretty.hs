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
            tail = "(" ++ csv ", " tailmap ++ ")"

instance Pretty Rule where 
    pretty (Rule term []) = pretty term ++ "."
    pretty (Rule term right) = pretty term ++ " :- " ++ (csv ", " . map pretty $ right)  ++ "."

instance Pretty Prog where 
    pretty (Prog rules) = csv "\n" . map pretty $ rules

instance Pretty Goal where


csv :: String -> [String] -> String
csv delimeter [] = ""
csv delimeter [x] = x
csv delimeter (x:xs) = x ++ delimeter ++ csv delimeter xs