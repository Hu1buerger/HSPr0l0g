module Main where

import Prelude (putStrLn, putStr, IO, Monad ((>>)))

import App.CommandLine

main :: IO ()
main = asciiArt >> startRepl


asciiArt :: IO ()
asciiArt = do 
    putStrLn ""
    putStrLn "**      **  ******** *******          ****   **  ****         "
    putStrLn "/**     /** **////// /**////**        *///** /** *///**  ***** "
    putStrLn "/**     /**/**       /**   /** ******/*  */* /**/*  */* **///**"
    putStrLn "/**********/*********/******* //**//*/* * /* /**/* * /*/**  /**"
    putStrLn "/**//////**////////**/**////   /** / /**  /* /**/**  /*//******"
    putStrLn "/**     /**       /**/**       /**   /*   /* /**/*   /* /////**"
    putStrLn "/**     /** ******** /**      /***   / ****  ***/ ****   ***** "
    putStrLn "//      // ////////  //       ///     ////  ///  ////   /////  "
    putStrLn "\n\n"
    putStrLn "\t 2 pains 1 program. A prolog interpreter written in haskell."
    putStr   "\n \n \n"
-- main = putStr $ applyRight x y ++ notInDom x y