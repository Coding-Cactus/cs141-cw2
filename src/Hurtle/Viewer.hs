module Hurtle.Viewer (renderHogoAnimation) where

import Hatch

import Control.Monad.State
import Hurtle.Types
import Data.Foldable (foldl')

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Fixed (mod')


renderHogoAnimation :: HogoProgram -> IO ()
renderHogoAnimation program = runAnimation finalState
  where
    finalState = drawState . runProgramUptoFrame program


drawState :: TurtleState -> Image
drawState turtle = foldl' (<@>) blank (map drawPolygon $ polygonsDrawnSoFar turtle)
               <@> foldl' (<@>) blank (map drawLine $ linesDrawnSoFar turtle)
               <@> turtleImg
  where
    drawLine ((x1, y1), (x2, y2), (r, g, b)) = applyColour r g b $ line x1 y1 x2 y2

    drawPolygon (path, (r, g, b)) = applyColour r g b $ polygon path

    turtleImg = offset (round x) (round y) $ rotate (round $ angle turtle) ant
    (x, y) = position turtle


runProgramUptoFrame :: HogoProgram -> Int -> TurtleState
runProgramUptoFrame program frames = execState (evalProgram program) initialTurtle
  where
    initialTurtle = TurtleState {
      position = (0, 0),
      angle = 0,
      penDown = True,
      colour = (0, 0, 0),
      linesDrawnSoFar = [],
      polygonsDrawnSoFar = [],
      currentPolygonDrawing = [],
      filling = False,
      remainingFrames = fromIntegral frames,
      speed = 10,
      symbolTable = Map.empty,
      subroutineTable = Map.empty
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
        GoForward      dist          -> forwardCommand       dist
        GoBackward     dist          -> backwardCommand      dist
        TurnRight      dtheta        -> turnRightCommand     dtheta
        TurnLeft       dtheta        -> turnLeftCommand      dtheta
        SetSpeed       speed         -> setSpeedCommand      speed
        Wait           duration      -> waitCommand          duration
        GoHome                       -> goHomeCommand
        PenUp                        -> penUpCommand
        PenDown                      -> penDownCommand
        StartFill                    -> startFillCommand
        EndFill                      -> endFillCommand
        Colour         r g b         -> colourCommand        r g b
        ClearScreen                  -> clearScreenCommand
        Repeat         n commands    -> repeatCommand        n commands
        Forever        commands      -> foreverCommand       commands
        Assignment     name value    -> assignmentStatement  name value
        Subroutine     name commands -> subroutineDefinition name commands
        SubroutineCall name          -> subroutineCall       name

  put $ f turtle


-- | Expression Evaluation
evaluate :: Expression -> Map String Float -> Float
evaluate (Raw val)              _    = val
evaluate (Variable name)        vars = if Map.member name vars then vars Map.! name else 0
evaluate (Negate expr)          v    = -(evaluate expr v)
evaluate (Id expr)              v    = evaluate expr v
evaluate (Exponent expr1 expr2) v    = evaluate expr1 v ** evaluate expr2 v
evaluate (Multiply expr1 expr2) v    = evaluate expr1 v * evaluate expr2 v
evaluate (Divide expr1 expr2)   v    = evaluate expr1 v / evaluate expr2 v
evaluate (Modulo expr1 expr2)   v    = evaluate expr1 v `mod'` evaluate expr2 v
evaluate (Plus expr1 expr2)     v    = evaluate expr1 v + evaluate expr2 v
evaluate (Minus expr1 expr2)    v    = evaluate expr1 v - evaluate expr2 v



-- | Drawing Commands
forwardCommand :: Expression -> TurtleState -> TurtleState
forwardCommand dist turtle = turtle {
    position = newPosition,
    linesDrawnSoFar = newLines,
    remainingFrames = newRemainingFrames,
    currentPolygonDrawing = newCurrentPolygonDrawing
  }
  where
    currentRemainingFrames = remainingFrames turtle
    newRemainingFrames = max 0 $ currentRemainingFrames - (evaluate dist (symbolTable turtle) / speed turtle)
    distCompletion = (currentRemainingFrames - newRemainingFrames) * speed turtle

    (x, y) = position turtle
    theta = pi / 180 * angle turtle

    newX = x + distCompletion * sin theta
    newY = y + distCompletion * cos theta
    newPosition = (newX, newY)

    currentLines = linesDrawnSoFar turtle
    newLines =
      if penDown turtle
        then ((x, y), (newX, newY), colour turtle) : currentLines
        else currentLines

    newCurrentPolygonDrawing =
      if filling turtle
        then (newX, newY) : currentPolygonDrawing turtle
        else currentPolygonDrawing turtle


backwardCommand :: Expression -> TurtleState -> TurtleState
backwardCommand dist = forwardCommand (Negate dist)


-- | Turning Commands
turnLeftCommand  :: Expression -> TurtleState -> TurtleState
turnRightCommand :: Expression -> TurtleState -> TurtleState

turnLeftCommand  dTheta turtle = turtle { angle = angle turtle - evaluate dTheta (symbolTable turtle) }
turnRightCommand dTheta turtle = turtle { angle = angle turtle + evaluate dTheta (symbolTable turtle) }


setSpeedCommand :: Expression -> TurtleState -> TurtleState
setSpeedCommand newSpeed turtle = turtle { speed = evaluate newSpeed (symbolTable turtle) }


waitCommand :: Expression -> TurtleState -> TurtleState
waitCommand duration turtle = turtle { remainingFrames = max 0 (remainingFrames turtle - (evaluate duration (symbolTable turtle) * 30)) }


goHomeCommand :: TurtleState -> TurtleState
goHomeCommand turtle = turtle { position = (0, 0), angle = 0 }


-- | Pen Commands
penUpCommand       :: TurtleState -> TurtleState
penDownCommand     :: TurtleState -> TurtleState
startFillCommand   :: TurtleState -> TurtleState
endFillCommand     :: TurtleState -> TurtleState
colourCommand      :: Expression -> Expression -> Expression -> TurtleState -> TurtleState
clearScreenCommand :: TurtleState -> TurtleState

penUpCommand turtle        = turtle { penDown = False }
penDownCommand turtle      = turtle { penDown = True }
colourCommand r g b turtle = turtle { colour = (floor $ evaluate r (symbolTable turtle),  floor $ evaluate g (symbolTable turtle), floor $ evaluate b (symbolTable turtle)) }
clearScreenCommand turtle  = turtle { linesDrawnSoFar = [], position = (0, 0) }
startFillCommand turtle    = turtle { filling = True, currentPolygonDrawing = [position turtle] }
endFillCommand turtle      = turtle {
  filling = False,
  polygonsDrawnSoFar = (currentPolygonDrawing turtle, colour turtle) : polygonsDrawnSoFar turtle,
  currentPolygonDrawing = []
}


-- | Control Flow commands
repeatCommand :: Expression -> HogoProgram -> TurtleState -> TurtleState
repeatCommand nExpr commands turtle
  | n == 0 || remainingFrames turtle <= 0 = turtle
  | otherwise = repeatCommand nMinusOne commands $ execState (evalProgram commands) turtle
  where
    n = evaluate nExpr (symbolTable turtle)
    nMinusOne = Raw (n - 1)

foreverCommand :: HogoProgram -> TurtleState -> TurtleState
foreverCommand commands turtle
  | remainingFrames turtle <= 0 = turtle
  | otherwise = foreverCommand commands $ execState (evalProgram commands) turtle


-- | Assignment Statement
assignmentStatement :: String -> Expression -> TurtleState -> TurtleState
assignmentStatement name value turtle = turtle { symbolTable = Map.insert name (evaluate value (symbolTable turtle)) $ symbolTable turtle  }

subroutineDefinition :: String -> HogoProgram -> TurtleState -> TurtleState
subroutineDefinition name commands turtle = turtle { subroutineTable = Map.insert name commands $ subroutineTable turtle }

subroutineCall :: String -> TurtleState -> TurtleState
subroutineCall name turtle = execState (evalProgram subroutine) turtle
  where
    subs = subroutineTable turtle
    subroutine
      | Map.member name subs = subroutineTable turtle Map.! name
      | otherwise = []
