module Hurtle.Parser (parseHogoFile, parseHogo) where

import Hurtle.Types

-- You'll probably want to refer to https://hackage.haskell.org/package/megaparsec for documentation of the Megaparsec library.
import Text.Megaparsec
import Text.Megaparsec.Char
    ( alphaNumChar, char, hspace1, string, space1, hspace )
import Text.Megaparsec.Char.Lexer
    ( decimal, float, skipLineComment, space )
import Control.Monad ( void )
import Control.Monad.Combinators.Expr
    ( makeExprParser, Operator(InfixL, Prefix, InfixR) )



{-
  This is the main way a program should interact with the hogo parser. You pass in a filename and program code and
  get either a string error or a HogoProgram. This means no programs using this function need to import any non-hogo
  packages in order to use this function.
-}
parseHogoFile :: String -> String -> Either String HogoProgram
parseHogoFile fname content =
  case parse parseHogo fname content of
    Left err      -> Left $ errorBundlePretty err
    Right program -> Right program


{-
  This is the complete Parser for hogo programs. It is exported from this module in order to be used in the tests.

  We start by skipping any whitespace (and comments), and then begin parsing the statements until the end of the file is
  reached. We need to initially skip whitespace outside of the statement parser because otherwise a program consisting
  of just whitespace would match the skipWhitespace as the first check in the statement parser, and then fail when the
  parser expects a command, but a program consisting of just whitespace is a valid program.
-}
parseHogo :: Parser HogoProgram
parseHogo = skipWhitespace >> manyTill parseStmt eof


-- Parse an individual hogo statement.
parseStmt :: Parser HogoCode
parseStmt = do
  statement <- nullaryCommand
           <|> unaryCommand
           <|> colourCommand
           <|> repeatCommand
           <|> foreverCommand
           <|> variableAssignment
           <|> subroutineDefinition
           <|> subroutineCall

  skipWhitespace -- skip whitespace in preparation for next statement
  pure statement



-- | Basic commands

{-
  This function parses the commands which take no arguments. This is simply involves checking the string matches the
  command and then returning the correct HogoCode constructor
-}
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


{-
  This function parses the commands which take a single argument. Here, we check the string matches and then parse the
  following expression which is the argument.
-}
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


{-
  The colour command is simply a command with three arguments, so follows the same pattern as nullary and unary. As
  there's only one trinary command in my hogo implementation, this is a custom function for parsing the colour command,
  however it could be easily extended into a general trinary parser if needed in future.
-}
colourCommand :: Parser HogoCode
colourCommand = do
      void $ string "colour"

      hspace1
      red <- expression

      hspace1
      green <- expression

      hspace1
      Colour red green <$> expression

{-
  For the repeat command, the command name is first matched as with all other commands. Then an expression is parsed,
  this is the number of loops which will occur. Then finally the block which will be repeated is parsed using the util
  `parseBlock` parser.
-}
repeatCommand :: Parser HogoCode
repeatCommand = do
  void $ string "repeat"
  hspace1

  num <- expression
  hspace1

  Repeat num <$> parseBlock

{-
  The form of the forever command is very similar to repeat, it's just missing the expression argument for the loop
  count
-}
foreverCommand :: Parser HogoCode
foreverCommand = do
  void $ string "forever"
  hspace1

  Forever <$> parseBlock



-- | User Defined Constructs


{-
  To parse variable assignment, firstly the identifier is parsed, then the equals sign (with any number of spaces
  surrounding it), then the expression to be assigned to the variable.
-}
variableAssignment :: Parser HogoCode
variableAssignment = do
  name <- variable

  void $ symbol "="

  Assignment name <$> expression


{-
  Subroutines are defined by writing "sub name [ <commands> ]". This is parsed by firstly matching the "sub", then the
  name. Then, the commands are parsed using the `parseBlock` until parser.
-}
subroutineDefinition :: Parser HogoCode
subroutineDefinition = do
  void $ string "sub"
  hspace1

  name <- subroutine

  skipWhitespace
  Subroutine name <$> parseBlock


-- My hogo subroutines don't take arguments, so parsing a subroutine call simply involves matching the name
subroutineCall :: Parser HogoCode
subroutineCall = SubroutineCall <$> subroutine



-- | Expressions
-- | The basis of this expression parsing is the example code from https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Expr.html

{-
  Creates an expression parser based off the operators table and definition of a term.
-}
expression :: Parser Expression
expression = makeExprParser term table <?> "expression"

{-
  A term within an expression can be one of:
    - A bracketed expression
    - A raw number
    - A variable
-}
term :: Parser Expression
term = parens expression <|> Raw <$> number <|> Variable <$> variable <?> "term"

{-
  This is the table of operators. It defines the precedence (highest first, least last) of each operator, and how they
  apply to their arguments.
-}
table :: [[Operator Parser Expression]]
table = [ [ prefixOp  "-" Negate
          , prefixOp  "+" Id ]
        , [ binaryOpR "^" Exponent ]
        , [ binaryOpL "*" Multiply
          , binaryOpL "/" Divide
          , binaryOpL "%" Modulo   ]
        , [ binaryOpL "+" Plus
          , binaryOpL "-" Minus  ] ]

-- Left associative infix binary operator
binaryOpL :: String -> (a -> a -> a) -> Operator Parser a
binaryOpL  name f = InfixL (f <$ symbol name)

-- Right associative infix binary operator
binaryOpR :: String -> (a -> a -> a) -> Operator Parser a
binaryOpR  name f = InfixR (f <$ symbol name)

-- Prefix operator
prefixOp :: String -> (a -> a) -> Operator Parser a
prefixOp  name f = Prefix (f <$ symbol name)

-- Applies a parser to the contents between a pair of open and closed brackets
parens :: Parser a -> Parser a
parens = between (symbol' "(") (symbol' ")")



-- | Util Parsers


-- Parse a string, consuming surrounding horizontal whitespace
symbol :: String -> Parser String
symbol str = try $ do
  hspace
  s <- string str
  hspace
  pure s

-- Parse a string, consuming only the preceding horizontal whitespace
symbol' :: String -> Parser String
symbol' str = try $ do
  hspace
  string str

-- Skip whitespace and comments. Skips across multiple lines, not just the current one.
skipWhitespace :: Parser ()
skipWhitespace = space space1 (skipLineComment ";") empty

-- Parse a number. Numbers can be written as integers or floats in the code (i.e. 5 and 5.5 are allowed).
number :: Parser Float
number = try float <|> decimal

-- Parse a variable name. Variables are formed by a colon followed by an alphanumeric string (e.g. :num).
variable :: Parser String
variable = do
  c <- char ':'
  name <- some alphaNumChar

  pure $ c : name

-- Parse a subroutine name. Subroutines are formed by a hash followed by an alphanumeric string (e.g. #circle)
subroutine :: Parser String
subroutine = do
  c <- char '#'
  name <- some alphaNumChar

  pure $ c : name

{-
  Parse a code block: [ <commands> ].

  This is done by firstly matching the opening square bracket, skipping any whitespace, and then applying `parseStmt`
  until the closing square bracket is found. The results of each `parseStmt` are collected into a list.
-}
parseBlock :: Parser HogoProgram
parseBlock = do
  void $ char '['
  skipWhitespace
  manyTill parseStmt (char ']')
