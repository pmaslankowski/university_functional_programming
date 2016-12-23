{-                 Functional programming 2016              -
 -    Interpreter of dynamically typed functional language: -
 -                    Mini Functional Language              -
 -                    Author: Piotr Maślankowski            -
 -                       Module: Interpreter                -
 -                     Interpreting given AST               -}

 module Interpreter where
   import Data.Map
   import Control.Monad.State
   import Parser

   data Number = Z | S Number
     deriving (Show, Eq)

   data Expression = Fun String ASTExpression
                   | Value Number
     deriving Show

   type Environment = Map String Expression
   type Interpreter a = State Environment a

   substitute :: String -> Expression -> Interpreter ()
   substitute var expr = state $ \env -> ((), insert var expr env)

   variable :: String -> Interpreter Expression
   variable var = state $ \env -> (env ! var, env)


   evaluate :: ASTExpression -> Interpreter Expression
   evaluate ast = case ast of
     Zero                                     -> return $ Value Z

     Succ expr                                -> do { eExpr <- evaluate expr
                                                    ; let Value eExpr' = eExpr in
                                                      return $ Value $ S eExpr' }

     Label lab                                -> variable lab

     Apply fun arg                            -> do { eArg <- evaluate arg
                                                    ; eFun <- evaluate fun
                                                    ; let Fun formalArg body = eFun in
                                                      do substitute formalArg eArg
                                                         evaluate body }

     Lambda arg body                          -> let Label eArg = arg in return $ Fun eArg body

     Case what target case1 (Label lab) case2 -> do { eWhat   <- evaluate what
                                                    ; eTarget <- evaluate target
                                                    ; let Value nWhat   = eWhat
                                                          Value nTarget = eTarget
                                                      in
                                                      if nWhat == nTarget then
                                                        evaluate case1
                                                      else
                                                        let S val = nWhat in
                                                        do substitute lab $ Value val
                                                           evaluate case2 }


   interpreter :: ASTExpression -> Expression
   interpreter ast = evalState (evaluate ast) empty

   test :: String -> Expression
   test code = case parser "" code of
     Right ast -> interpreter ast
