module App.Main where

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
    putStr   "\n \n \n"
-- main = putStr $ applyRight x y ++ notInDom x y