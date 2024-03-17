module Hurtle.Parser (parseHogoFile, parseHogo) where

import Hurtle.Types

-- You'll probably want to refer to https://hackage.haskell.org/package/megaparsec for documentation of the Megaparsec library.
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Control.Monad ( void, when )


parseHogoFile :: String -> String -> Either String HogoProgram
parseHogoFile fname content =
  case parse parseHogo fname content of
    Left err      -> Left $ errorBundlePretty err
    Right program -> Right program


parseHogo :: Parser HogoProgram
parseHogo = commentsAndWhitespace >> manyTill parseStmt eof

parseStmt :: Parser HogoCode
parseStmt = do
  statement <- nullaryCommand
           <|> unaryCommand
           <|> colourCommand
           <|> repeatCommand
           <|> foreverCommand

  commentsAndWhitespace
  pure statement


commentsAndWhitespace :: Parser ()
commentsAndWhitespace = skipMany (space1 <|> skipLineComment ";")

number :: Parser Float
number = try float <|> decimal


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
      stmtType <$> number


colourCommand :: Parser HogoCode
colourCommand = do
      void $ string "colour"

      hspace1
      red <- decimal
      when (red < 0 || red > 255) $ fail "red value must be in 0..255"

      hspace1
      green <- decimal
      when (green < 0 || green > 255) $ fail "green value must be in 0..255"

      hspace1
      blue <- decimal
      when (blue < 0 || blue > 255) $ fail "blue value must be in 0..255"

      pure $ Colour red green blue


repeatCommand :: Parser HogoCode
repeatCommand = do
  void $ string "repeat"
  hspace1
  num <- decimal
  hspace1
  Repeat num <$> parseBlock


foreverCommand :: Parser HogoCode
foreverCommand = do
  void $ string "forever"
  hspace1
  Forever <$> parseBlock


parseBlock :: Parser HogoProgram
parseBlock = do
  void $ char '['
  commentsAndWhitespace
  manyTill parseStmt (char ']')

