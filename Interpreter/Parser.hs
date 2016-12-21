{-                 Functional programming 2016              -
 -    Interpreter of dynamically typed functional language: -
 -                    Mini Functional Language              -
 -                    Author: Piotr Maślankowski            -
 -                       Module: Parser                     -
 -       Parsing given source code to Abstract Syntax Tree  -}

module Parser (parser) where
  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Expr
  import Text.ParserCombinators.Parsec.Language
  import qualified Text.ParserCombinators.Parsec.Token as Token

  type Filename = String
  type Code     = String

  {- AST datatype -}
  data Expression = Let Expression Expression Expression
                  | Lambda Expression Expression
                  | Case Expression Expression Expression Expression Expression
                  | Apply Expression Expression
                  | Label String
                  | Succ Expression
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
             , Token.reservedOpNames = ["\\", "->", ":"]
             }

  lexer = Token.makeTokenParser mflDef

  {-Token parsers -}
  identifier = Token.identifier lexer
  reserved   = Token.reserved   lexer
  reservedOp = Token.reservedOp lexer
  parens     = Token.parens     lexer
  integer    = Token.integer    lexer
  whiteSpace = Token.whiteSpace lexer


  sflParser :: Parser Expression
  sflParser = whiteSpace >> expression

  expression :: Parser Expression
  expression =  letExpr
            <|> caseExpr
            <|> numberExpr
            <|> succExpr
            <|> try applyExpr    -- can fail after consuming "("
            <|> labelExpr
            <|> try functionExpr -- can fail after consuming "("
            <|> parenthesisExpr

  letExpr :: Parser Expression
  letExpr = do reserved "let"
               lab <- identifier
               reserved "="
               bindExpr <- expression
               reserved "in"
               expr <- expression
               return $ Let (Label lab) bindExpr expr

  caseExpr :: Parser Expression
  caseExpr = do reserved "case"
                expr <- expression
                reserved "of"
                num <- numberExpr
                reservedOp ":"
                expr1 <- expression
                reserved "!"
                lab <- identifier
                reservedOp ":"
                expr2 <- expression
                return $ Case expr num expr1 (Label lab) expr2

  functionExpr :: Parser Expression
  functionExpr =  parens functionExpr
              <|> do reservedOp "\\"
                     var <- identifier
                     reservedOp "->"
                     expr <- expression
                     return $ Lambda (Label var) expr

  succExpr :: Parser Expression
  succExpr = do reserved "!"
                expr <- expression
                return $ Succ expr

  numberExpr :: Parser Expression
  numberExpr =  do { num <- integer; return $ fromInt num }
                  where fromInt n = if n == 0 then Zero else Succ $ fromInt $ n-1

  labelExpr :: Parser Expression
  labelExpr = do lab <- identifier
                 return $ Label lab

  applyExpr :: Parser Expression
  applyExpr = do fun <- labelExpr <|> functionExpr
                 argument <- expression
                 return $ Apply fun argument

  parenthesisExpr :: Parser Expression
  parenthesisExpr = parens expression


  -- main parser function
  -- Function returns AST or error message from given source code in SFL
  -- arguments:
  -- filename - name of file which is being parsed (this will be used only in case of errors)
  -- code - source code to parse
  parser :: Filename -> Code -> Either ParseError Expression
  parser = parse sflParser
