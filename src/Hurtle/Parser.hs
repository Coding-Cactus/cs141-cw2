module Hurtle.Parser (parseHogoFile, parseHogo) where

import Hurtle.Types ( Parser, HogoCode(..), HogoProgram )

-- You'll probably want to refer to https://hackage.haskell.org/package/megaparsec for documentation of the Megaparsec library.
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Control.Monad ( void )


parseHogoFile :: String -> String -> Either String HogoProgram
parseHogoFile fname content =
  case parse parseHogo fname content of
    Left err      -> Left $ errorBundlePretty err
    Right program -> Right program


parseHogo :: Parser HogoProgram
parseHogo = commentsAndWhitespace >> manyTill parseStmt eof

parseStmt :: Parser HogoCode
parseStmt = do
  statement <- nullaryCommand <|> unaryCommand <|> repeatCommand
  commentsAndWhitespace
  pure statement


commentsAndWhitespace :: Parser ()
commentsAndWhitespace = skipMany (space1 <|> skipLineComment ";")


nullaryCommand :: Parser HogoCode
nullaryCommand = command "home"        GoHome
             <|> command "penup"       PenUp
             <|> command "pendown"     PenDown
             <|> command "clearscreen" ClearScreen
  where
    command :: String -> HogoCode -> Parser HogoCode
    command cmd stmtType = do
      void $ string cmd
      pure stmtType



unaryCommand :: Parser HogoCode
unaryCommand = command "forward" GoForward
           <|> command "back"    GoBackward
           <|> command "left"    TurnLeft
           <|> command "right"   TurnRight
           <|> command "speed"   SetSpeed
  where
    command :: String -> (Float -> HogoCode) -> Parser HogoCode
    command cmd stmtType = do
      void $ string cmd
      hspace1
      stmtType <$> (try float <|> decimal)



repeatCommand :: Parser HogoCode
repeatCommand = do
  void $ string "repeat"
  hspace1
  num <- decimal
  hspace1
  Repeat num <$> parseRepeatBlock
  where
    parseRepeatBlock :: Parser HogoProgram
    parseRepeatBlock = do
      void $ char '['
      commentsAndWhitespace
      manyTill parseStmt (char ']')
