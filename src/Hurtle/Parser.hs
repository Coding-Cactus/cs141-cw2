module Hurtle.Parser (parseHogoFile, parseHogo) where

import Hurtle.Types

-- You'll probably want to refer to https://hackage.haskell.org/package/megaparsec for documentation of the Megaparsec library.
import Text.Megaparsec
    ( (<|>),
      (<?>),
      parse,
      errorBundlePretty,
      between,
      manyTill,
      choice,
      some,
      MonadParsec(try, eof) )
import Text.Megaparsec.Char
    ( alphaNumChar, char, hspace1, string, space1, hspace )
import Text.Megaparsec.Char.Lexer
    ( decimal, float, skipLineComment, space )
import Control.Monad ( void )
import Control.Monad.Combinators.Expr
    ( makeExprParser, Operator(InfixL, Prefix, InfixR) )
import Control.Applicative (empty)


parseHogoFile :: String -> String -> Either String HogoProgram
parseHogoFile fname content =
  case parse parseHogo fname content of
    Left err      -> Left $ errorBundlePretty err
    Right program -> Right program


parseHogo :: Parser HogoProgram
parseHogo = skipWhitespace >> manyTill parseStmt eof

parseStmt :: Parser HogoCode
parseStmt = do
  statement <- nullaryCommand
           <|> unaryCommand
           <|> colourCommand
           <|> repeatCommand
           <|> foreverCommand
           <|> variableAssignment

  skipWhitespace
  pure statement


nullaryCommand :: Parser HogoCode
nullaryCommand = command "home"        GoHome
             <|> command "penup"       PenUp
             <|> command "pendown"     PenDown
             <|> command "startfill"   StartFill
             <|> command "endfill"     EndFill
             <|> command "clearscreen" ClearScreen
  where
    command cmd stmtType = do
      void $ string cmd
      pure stmtType



unaryCommand :: Parser HogoCode
unaryCommand = command "forward" GoForward
           <|> command "back"    GoBackward
           <|> command "left"    TurnLeft
           <|> command "right"   TurnRight
           <|> command "speed"   SetSpeed
           <|> command "wait"    Wait
  where
    command cmd stmtType = do
      void $ string cmd
      hspace1
      stmtType <$> expression


colourCommand :: Parser HogoCode
colourCommand = do
      void $ string "colour"

      hspace1
      red <- expression

      hspace1
      green <- expression

      hspace1
      Colour red green <$> expression


repeatCommand :: Parser HogoCode
repeatCommand = do
  void $ string "repeat"
  hspace1
  num <- expression
  hspace1
  Repeat num <$> parseBlock


foreverCommand :: Parser HogoCode
foreverCommand = do
  void $ string "forever"
  hspace1
  Forever <$> parseBlock


parseBlock :: Parser HogoProgram
parseBlock = do
  --between (sym "[") (sym "]") parseHogo
  void $ char '['
  skipWhitespace
  manyTill parseStmt (char ']')



variableAssignment :: Parser HogoCode
variableAssignment = do
  name <- identifier

  hspace1
  void $ string "="
  hspace1

  Assignment name <$> expression



-- | Expressions

expression :: Parser Expression
expression = makeExprParser term table <?> "expression"

term :: Parser Expression
term = choice (map try [ parens expression, Raw <$> number, Variable <$> identifier ]) <?> "term"

table :: [[Operator Parser Expression]]
table = [ [ prefixOp  "-" Negate
          , prefixOp  "+" Id ]
        , [ binaryOpR "^" Exponent ]
        , [ binaryOpL "*" Multiply
          , binaryOpL "/" Divide
          , binaryOpL "%" Modulo   ]
        , [ binaryOpL "+" Plus
          , binaryOpL "-" Minus  ] ]

binaryOpL :: String -> (a -> a -> a) -> Operator Parser a
binaryOpL  name f = InfixL (f <$ symbol name)

binaryOpR :: String -> (a -> a -> a) -> Operator Parser a
binaryOpR  name f = InfixR (f <$ symbol name)

prefixOp :: String -> (a -> a) -> Operator Parser a
prefixOp  name f = Prefix (f <$ symbol name)

parens :: Parser a -> Parser a
parens = between (symbol' "(") (symbol' ")")



-- | Util Parsers

symbol :: String -> Parser String
symbol str = try $ do
  hspace
  s <- string str
  hspace
  pure s

symbol' :: String -> Parser String
symbol' str = try $ do
  hspace
  string str

skipWhitespace :: Parser ()
skipWhitespace = space space1 (skipLineComment ";") empty

number :: Parser Float
number = try float <|> decimal

identifier :: Parser String
identifier = do
  c <- char ':'
  name <- some alphaNumChar

  pure $ c : name
