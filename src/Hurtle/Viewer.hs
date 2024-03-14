module Hurtle.Viewer (renderHogoAnimation) where

import Hatch

import Control.Monad.State
import Hurtle.Types
import Data.Foldable (foldl')




renderHogoAnimation :: HogoProgram -> IO ()
renderHogoAnimation program = runAnimation finalState
  where
    finalState = drawState . runProgramUptoFrame program


drawState :: TurtleState -> Image
drawState turtle = foldl' (<@>) turtleImg $ map drawLine $ linesDrawnSoFar turtle
  where
    drawLine ((x1, y1), (x2, y2)) = line x1 y1 x2 y2
    turtleImg = offset (round x) (round y) $ rotate (round $ angle turtle) ant
    (x, y) = position turtle


runProgramUptoFrame :: HogoProgram -> Int -> TurtleState
runProgramUptoFrame program frames = execState (evalProgram program) initialTurtle
  where
    initialTurtle = TurtleState {
      position = (0, 0),
      angle = 0,
      penDown = True,
      linesDrawnSoFar = [],
      remainingFrames = fromIntegral frames,
      speed = 10
    }

evalProgram :: HogoProgram -> State TurtleState ()
evalProgram program = do
  turtle <- get

  if remainingFrames turtle <= 0 || null program then
    pure ()
  else do
    runCommand $ head program
    evalProgram $ tail program

runCommand :: HogoCode -> State TurtleState ()
runCommand command = do
  turtle <- get

  let
    f =
      case command of
        GoForward   dist          -> forwardCommand      dist
        GoBackward  dist          -> backwardCommand     dist
        TurnRight   dtheta        -> turnRightCommand    dtheta
        TurnLeft    dtheta        -> turnLeftCommand     dtheta
        SetSpeed    speed         -> setSpeedCommand     speed
        GoHome                    -> goHomeCommand
        PenUp                     -> penUpCommand
        PenDown                   -> penDownCommand
        ClearScreen               -> clearScreenCommand
        Repeat       n   commands -> repeatCommand       n    commands

  put $ f turtle

-- | Drawing Commands
forwardCommand :: Float -> TurtleState -> TurtleState
forwardCommand dist turtle = turtle {
    position = newPosition,
    linesDrawnSoFar = newLines,
    remainingFrames = newRemainingFrames
  }
  where
    currentRemainingFrames = remainingFrames turtle
    newRemainingFrames = max 0 $ currentRemainingFrames - (dist / speed turtle)
    distCompletion = (currentRemainingFrames - newRemainingFrames) * speed turtle

    (x, y) = position turtle
    theta = pi / 180 * angle turtle

    newX = x + distCompletion * sin theta
    newY = y + distCompletion * cos theta
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


-- | Speed Command
setSpeedCommand :: Float -> TurtleState -> TurtleState
setSpeedCommand newSpeed turtle = turtle { speed = newSpeed }


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
repeatCommand n commands turtle
  | n == 0 || remainingFrames turtle <= 0 = turtle
  | otherwise = repeatCommand (n-1) commands $ execState (evalProgram commands) turtle
