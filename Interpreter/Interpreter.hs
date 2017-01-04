{-                 Functional programming 2016              -
 -    Interpreter of dynamically typed functional language: -
 -                    Mini Functional Language              -
 -                    Author: Piotr Maślankowski            -
 -                       Module: Interpreter                -
 -                     Interpreting given AST               -}

module Interpreter where
  import Data.Map
  import Control.Monad.State
  import Control.Monad.Except
  import Parser

  data Number = Z | S Number
    deriving (Show, Eq)

  -- result datatype
  data Expression = Fun String ASTExpression
                  | Value Number
    deriving Show

  type Environment = Map String Expression
  type Interpreter = ExceptT String (State Environment)

  -- Function substitutes second argument as variable name given as first argument
  substitute :: String -> Expression -> Interpreter ()
  substitute var expr = do env <- get
                           put $ insert var expr env

  -- Function gets value of given variable name
  variable :: String -> Interpreter Expression
  variable var = do env <- get
                    case Data.Map.lookup var env of
                      Nothing -> throwError $ "Variable not in scope: " ++ var
                      Just x  -> return x


  -- Function evaluates value of given AST
  evaluate :: ASTExpression -> Interpreter Expression
  evaluate ast = case ast of
    Zero                     -> return $ Value Z

    Succ expr                -> do { eExpr <- evaluate expr
                                   ; case eExpr of
                                       Value eExpr' -> return $ Value $ S eExpr'
                                       Fun _ _       ->
                                         throwError "Attempt to take succesor of function" }

    Label lab               -> variable lab

    Apply fun arg           -> do { env <- get
                                  ; eArg <- evaluate arg
                                  ; put env
                                  ; eFun <- evaluate fun
                                  ; case eFun of
                                      Value _            -> throwError "Attempt to apply number"
                                      Fun formalArg body -> do substitute formalArg eArg
                                                               evaluate body }

    Lambda (Label arg) body -> return $ Fun arg body

    Case what target case1 (Label lab) case2 ->
      do { eWhat   <- evaluate what
         ; eTarget <- evaluate target
         ; case (eWhat, eTarget) of
             (Value nWhat, Value nTarget) ->
               if nWhat == nTarget then
                 evaluate case1
               else case nWhat of
                      S val -> do substitute lab $ Value val
                                  evaluate case2
                      Z  -> throwError "Attempt to find x such that S x = 0 in case instruction"

             _ -> throwError "Attempt to make case instruction on functions." }


  interpreter :: ASTExpression -> Either String Expression
  interpreter ast = evalState (runExceptT $ evaluate ast) empty


  normalizeOutput :: Either String Expression -> String
  normalizeOutput res =
    let normalizeNumber Z  = 0 :: Int
        normalizeNumber (S x) = 1 + normalizeNumber x
        -- (auxilary function transforms number to decimal notation)
    in case res of
      Left  msg        -> "<error>" ++ msg
      Right expression -> case expression of
                            Value num -> show $ normalizeNumber num
                            Fun _ _   -> "<fun>"


  test :: String -> IO ()
  test code = case parser "" code of
    Right ast -> print $ runState (runExceptT $ evaluate ast) empty
    Left msg -> print msg
