module Hurtle.Viewer (renderHogo) where

import Hatch

import Control.Monad.State
import Hurtle.Types
import Data.Foldable (foldl')

renderHogo :: HogoProgram -> IO ()
renderHogo program = runAnimation $ const finalState
  where
    finalState :: Image
    finalState = drawState $ runProgram program


drawState :: TurtleState -> Image
drawState turtle = foldl' (<@>) turtleImg $ map drawLine $ linesDrawnSoFar turtle
  where
    drawLine ((x1, y1), (x2, y2)) = line x1 y1 x2 y2
    turtleImg = rotate (round $ angle turtle) ant


runProgram :: HogoProgram -> TurtleState
runProgram program = execState (evalProgram program) initialTurtle
  where
    initialTurtle = TurtleState {
      position = (0, 0),
      angle = 0,
      penDown = True,
      linesDrawnSoFar = []
    }

evalProgram :: HogoProgram -> State TurtleState [()]
evalProgram = traverse runCommand

runCommand :: HogoCode -> State TurtleState ()
runCommand command = do
  turtle <- get

  let
    f =
      case command of
        GoForward   dist          -> forwardCommand      dist
        GoBackward  dist          -> backwardCommand     dist
        TurnRight   dθ            -> turnRightCommand    dθ
        TurnLeft    dθ            -> turnLeftCommand     dθ
        GoHome                    -> goHomeCommand
        PenUp                     -> penUpCommand
        PenDown                   -> penDownCommand
        ClearScreen               -> clearScreenCommand
        Repeat       n   commands -> repeatCommand       n    commands

  put $ f turtle

-- | Drawing Commands
forwardCommand :: Float -> TurtleState -> TurtleState
forwardCommand dist turtle = turtle { position = newPosition, linesDrawnSoFar = newLines }
  where
    (x, y) = position turtle
    theta = pi / 180 * angle turtle

    newX = x + dist * sin theta
    newY = y + dist * cos theta
    newPosition = (newX, newY)

    currentLines = linesDrawnSoFar turtle
    newLines =
      if penDown turtle
        then ((x, y), (newX, newY)) : currentLines
        else currentLines

backwardCommand :: Float -> TurtleState -> TurtleState
backwardCommand dist = forwardCommand (-dist)


-- | Turning Commands
turnLeftCommand  :: Float -> TurtleState -> TurtleState
turnRightCommand :: Float -> TurtleState -> TurtleState

turnLeftCommand  dTheta turtle = turtle { angle = angle turtle - dTheta }
turnRightCommand dTheta turtle = turtle { angle = angle turtle + dTheta }


-- | Home Command
goHomeCommand :: TurtleState -> TurtleState
goHomeCommand turtle = turtle { position = (0, 0), angle = 0 }


-- | Pen Commands
penUpCommand       :: TurtleState -> TurtleState
penDownCommand     :: TurtleState -> TurtleState
clearScreenCommand :: TurtleState -> TurtleState

penUpCommand turtle       = turtle { penDown = False }
penDownCommand turtle     = turtle { penDown = True }
clearScreenCommand turtle = turtle { linesDrawnSoFar = [] }


-- | Repeat Command
repeatCommand :: Int -> HogoProgram -> TurtleState -> TurtleState
repeatCommand n commands turtle = iterate (execState (evalProgram commands)) turtle !! n
