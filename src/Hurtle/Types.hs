module Hurtle.Types where

import Text.Megaparsec (Parsec)
import Data.Void (Void)
import Data.Map (Map)

--------------------------------------------------------------------------------
-- Type Definitions

-- | A Hogo program is a list of HogoCode instructions.
type HogoProgram = [HogoCode]

data HogoCode
  -- | Movement Commands
  = GoForward Expression
  | GoBackward Expression
  | TurnLeft Expression
  | TurnRight Expression
  | SetSpeed Expression
  | Wait Expression
  | GoHome
  -- | Pen Commands
  | PenUp
  | PenDown
  | StartFill
  | EndFill
  | Colour Expression Expression Expression
  | ClearScreen
  -- | Control Flow
  | Repeat Expression HogoProgram
  | Forever HogoProgram
  -- | Assignment
  | Assignment String Expression
  | Subroutine String HogoProgram
  | SubroutineCall String
  deriving (Show, Read, Eq)

data Expression
  = Raw Float
  | Variable String
  | Negate Expression
  | Id Expression
  | Exponent Expression Expression
  | Multiply Expression Expression
  | Divide Expression Expression
  | Modulo Expression Expression
  | Plus Expression Expression
  | Minus Expression Expression
  deriving (Show, Read, Eq)

data TurtleState = TurtleState {
  position :: (Float, Float),
  angle :: Float,
  penDown :: Bool,
  linesDrawnSoFar :: [((Float, Float), (Float, Float), (Int, Int, Int))], -- ((x1, y1), (x2, y2), (r, g, b))
  polygonsDrawnSoFar :: [([(Float, Float)], (Int, Int, Int))],
  currentPolygonDrawing :: [(Float, Float)],
  remainingFrames :: Float,
  speed :: Float,
  colour :: (Int, Int, Int),
  filling :: Bool,
  symbolTable :: Map String Float,
  subroutineTable :: Map String HogoProgram
}

-- | This is an alias for the Megaparsec parser type; the "Void" tells it that we don't have any custom error type, and the "string" tells it that we're parsing strings.
type Parser = Parsec Void String
