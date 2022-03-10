module 
 App.Pretty
 where 

import Data.List

import App.Type

-- how long i was searching in the prelude for this one... morto al Rusujo, nur ŝerce
class Show a => Pretty a where
    pretty :: a -> String

instance Pretty Term where
    pretty (Var (VarName name)) = name

    pretty (Comb cmbname []) = cmbname
    pretty (Comb cmbname combitems) = cmbname ++ tailRename
        where 
            tailmap = map (pretty) combitems
            tailRename = "(" ++ intercalate ", " tailmap ++ ")"

instance Pretty Rule where 
    pretty (Rule term []) = pretty term ++ "."
    pretty (Rule term right) = pretty term ++ " :- " ++ (intercalate ", " . map pretty $ right)  ++ "."

instance Pretty Prog where 
    pretty (Prog rules) = intercalate "\n" . map pretty $ rules

instance Pretty Goal where
    pretty (Goal goals) = "?- " ++ (intercalate ", " . map pretty $ goals) ++ "." 