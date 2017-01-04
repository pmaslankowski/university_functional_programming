{-                 Functional programming 2016              -
 -    Interpreter of dynamically typed functional language: -
 -                    Mini Functional Language              -
 -                    Author: Piotr Maślankowski            -
 -                       Module: Parser                     -
 -       Parsing given source code to Abstract Syntax Tree  -}

module Parser ( parser, ASTExpression(..) ) where
  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Expr
  import Text.ParserCombinators.Parsec.Language
  import qualified Text.ParserCombinators.Parsec.Token as Token

  type Filename = String
  type Code     = String

  {- AST datatype -}
  data ASTExpression = Lambda ASTExpression ASTExpression
                     | Case ASTExpression ASTExpression ASTExpression ASTExpression ASTExpression
                     | Apply ASTExpression ASTExpression
                     | Label String
                     | Succ ASTExpression
                     | Zero
    deriving Show

  {- Mini Functional Language definition: -}
  mflDef =
    emptyDef { Token.commentStart    = "{"
             , Token.commentEnd      = "}"
             , Token.commentLine     = "--"
             , Token.identStart      = letter
             , Token.identLetter     = alphaNum
             , Token.reservedNames   = [ "let"
                                       , "in"
                                       , "case"
                                       , "of", "!"]
             , Token.reservedOpNames = ["\\", "->", ":", ";", ","]
             }

  lexer = Token.makeTokenParser mflDef

  {-Token parsers -}
  identifier = Token.identifier lexer
  reserved   = Token.reserved   lexer
  reservedOp = Token.reservedOp lexer
  parens     = Token.parens     lexer
  integer    = Token.integer    lexer
  whiteSpace = Token.whiteSpace lexer


  sflParser :: Parser ASTExpression
  sflParser = whiteSpace >> expression

  expression :: Parser ASTExpression
  expression =  letExpr
            <|> caseExpr
            <|> numberExpr
            <|> succExpr
            <|> try applyExpr    -- can fail after consuming "("
            <|> labelExpr
            <|> try functionExpr -- can fail after consuming "("
            <|> parenthesisExpr

  letExpr :: Parser ASTExpression
  letExpr = do reserved "let"
               lab <- labelExpr
               reserved "="
               bindExpr <- expression
               reserved "in"
               expr <- expression
               {-return $ Let (Label lab) bindExpr expr-}
               return $ Apply (Lambda lab expr) bindExpr

  caseExpr :: Parser ASTExpression
  caseExpr = do reserved "case"
                expr <- expression
                reserved "of"
                what <- expression
                reservedOp ":"
                expr1 <- expression
                reservedOp ";"
                reserved "!"
                lab <- identifier
                reservedOp ":"
                expr2 <- expression
                return $ Case expr what expr1 (Label lab) expr2

  functionExpr :: Parser ASTExpression
  functionExpr =  try (parens functionExpr)
              <|> parens applyExpr
              <|> do reservedOp "\\"
                     var <- identifier
                     reservedOp "->"
                     expr <- expression
                     return $ Lambda (Label var) expr

  succExpr :: Parser ASTExpression
  succExpr = do reserved "!"
                expr <- expression
                return $ Succ expr

  numberExpr :: Parser ASTExpression
  numberExpr =  do { num <- integer
                   ; if num < 0 then unexpected "Unexpected -" else return $ fromInt num }
                  where fromInt n = if n == 0 then Zero else Succ $ fromInt $ n-1

  labelExpr :: Parser ASTExpression
  labelExpr = do lab <- identifier
                 return $ Label lab

  applyExpr :: Parser ASTExpression
  applyExpr = let apply acc [x] = Apply acc x
                  apply acc (x:xs) = apply (Apply acc x) xs
              in do fun <- labelExpr <|> functionExpr
                    arguments <- argsExpr
                    return $ apply fun arguments

  argsExpr :: Parser [ASTExpression]
  argsExpr = many1 argExpr

  argExpr :: Parser ASTExpression
  argExpr = parens expression <|> numberExpr <|> labelExpr

  parenthesisExpr :: Parser ASTExpression
  parenthesisExpr = parens expression


  -- main parser function
  -- Function returns AST or error message from given source code in SFL
  -- arguments:
  -- filename - name of file which is being parsed (this will be used only in case of errors)
  -- code - source code to parse
  parser :: Filename -> Code -> Either ParseError ASTExpression
  parser = parse (sflParser <* eof)
