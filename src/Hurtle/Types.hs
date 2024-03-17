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
  | StartFill
  | EndFill
  | Colour Int Int Int
  | ClearScreen
  -- | Control Flow
  | Repeat Int HogoProgram
  | Forever HogoProgram
  deriving (Show,Read,Eq)

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
  filling :: Bool
}

-- | This is an alias for the Megaparsec parser type; the "Void" tells it that we don't have any custom error type, and the "string" tells it that we're parsing strings.
type Parser = Parsec Void String
