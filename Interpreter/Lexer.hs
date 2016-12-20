{-                 Functional programming 2016             -
 -    Interpreter of dynamically typed functional language -
 -                    Author: Piotr Maślankowski           -
 -                       Module: Lexer                     -
 -       Lexical analysis of given program.                -}
module Lexer (lexer) where
  import Data.Char
  import Text.ParserCombinators.Parsec hiding (token)

  data Token = TokenLet
             | TokenIn
             | TokenArrow
             | TokenIdentifier String
             | TokenNumber Int
             | TokenCase
             | TokenOf
             | TokenEq
             | TokenLP
             | TokenRP
             | TokenLambda
             | TokenSucc
    deriving Show

  type Code = String
  type Filename = String

  word :: Parser Token
  word = do
    first <- letter
    rest <- many (letter <|> digit)
    let curr = first : rest
    return $ case curr of
      "let"  -> TokenLet
      "in"   -> TokenIn
      "case" -> TokenCase
      "of"   -> TokenOf
      other  -> TokenIdentifier other

  number :: Parser Token
  number = do
    first <- digit
    rest  <- many digit
    let num = read (first:rest) :: Int
    return $ TokenNumber num

  arrow :: Parser Token
  arrow = do
    string "->"
    return TokenArrow

  symbol :: Parser Token
  symbol = do
    curr <- oneOf "\\()=!"
    case curr of
      '\\' -> return TokenLambda
      '('  -> return TokenLP
      ')'  -> return TokenRP
      '='  -> return TokenEq
      '!'  -> return TokenSucc

  token :: Parser Token
  token = do
    spaces
    word <|> number <|> symbol <|> arrow

  program :: Parser [Token]
  program = many token

  {- Function returns list of tokens in given program or error masage.
     Arguments:
       - fname - name of proccessed file. This is used only in case of parsing
                 errors to return appropriate message.
       - code  - code to proccess
     Function returns value of type Either String [Token]. (Right something) means that
     everything went well, (Left msg) means that something described in msg went wrong. -}
  lexer :: Filename -> Code -> Either ParseError [Token]
  lexer = parse program
