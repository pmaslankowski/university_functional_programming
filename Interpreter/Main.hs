{-                 Functional programming 2016              -
 -    Interpreter of dynamically typed functional language: -
 -                    Mini Functional Language              -
 -                    Author: Piotr Maślankowski            -
 -                       Module: Main                       -
 -                       Main program                       -}

module Main where
  import Parser
  import Interpreter

  type Filename = String
  type Code     = String


  readCode :: Filename -> IO Code
  readCode = readFile


  runCode :: Filename -> Code -> String
  runCode fname code =
    case parser fname code of
      Left  msg -> "Parse error:\n" ++ show msg
      Right ast -> normalizeOutput $ interpreter ast

  ast fname = do code <- readCode fname
                 print $ parser fname code
  main :: IO ()
  main = do code <- readCode "multiplication.sfl"
            putStrLn $ runCode "mult.sfl" code
