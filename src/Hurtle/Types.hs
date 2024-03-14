module Hurtle.Types where

import Text.Megaparsec
import Data.Void

--------------------------------------------------------------------------------
-- Type Definitions

-- | A Hogo program is a list of HogoCode instructions.
type HogoProgram = [HogoCode]

data HogoCode
  -- | Movement Commands
  = GoForward Float
  | GoBackward Float
  | TurnLeft Float
  | TurnRight Float
  | SetSpeed Float
  | Wait Float
  | GoHome
  -- | Pen Commands
  | PenUp
  | PenDown
  | Colour Float Float Float
  | ClearScreen
  -- | Control Flow
  | Repeat Int HogoProgram
  | Forever HogoProgram
  deriving (Show,Read,Eq)

data TurtleState = TurtleState {
  position :: (Float, Float),
  angle :: Float,
  penDown :: Bool,
  linesDrawnSoFar :: [((Float, Float), (Float, Float), (Float, Float, Float))], -- ((x1, y1), (x2, y2), (r, g, b))
  remainingFrames :: Float,
  speed :: Float,
  colour :: (Float, Float, Float)
}

-- | This is an alias for the Megaparsec parser type; the "Void" tells it that we don't have any custom error type, and the "string" tells it that we're parsing strings.
type Parser = Parsec Void String
