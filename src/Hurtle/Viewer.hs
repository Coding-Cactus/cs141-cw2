module Hurtle.Viewer (renderHogoAnimation) where

import Hatch

import Hurtle.Types

import Control.Monad.State
import Data.Foldable (foldl')
import Data.Fixed (mod')

import Data.Map (Map)
import qualified Data.Map as Map

{-
  Renders the hogo program animation using (slightly modified) Hatch.

  `runAnimation` takes an animation function as an argument. This animation function is expected to produce the current
  state of the animation at a given frame. To do this, `drawStateAtFrame` runs the program up to the given frame and
  then draws that reached state.
-}
renderHogoAnimation :: HogoProgram -> IO ()
renderHogoAnimation program = runAnimation drawStateAtFrame
  where
    drawStateAtFrame = drawState . runProgramUpToFrame program


{-
  Draws the given state of the program.

  Polygons are drawn using the gloss polygon function. Unfortunately this function only guarantees the correct drawing
  of convex polygons, so concave polygons may see some unexpected results.
-}
drawState :: TurtleState -> Image
drawState turtle = foldl' (<@>) blank (map drawPolygon $ polygonsDrawnSoFar turtle)
               <@> foldl' (<@>) blank (map drawLine $ linesDrawnSoFar turtle)
               <@> turtleImg
  where
    drawPolygon (path, (r, g, b)) = applyColour r g b $ polygon path

    drawLine ((x1, y1), (x2, y2), (r, g, b)) = applyColour r g b $ line x1 y1 x2 y2

    turtleImg = offset (round x) (round y) $ rotate (round $ angle turtle) ant
    (x, y) = position turtle


{-
  Runs the hogo program up to a given frame. The program evaluation is a series of stateful computations, so the final
  state is computed by running `execState` with the initial turtle state.
-}
runProgramUpToFrame :: HogoProgram -> Int -> TurtleState
runProgramUpToFrame program frames = execState (evalProgram program) initialTurtle
  where
    -- Set up the turtle state with default values
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

-- Runs a hogo program's commands until it either runs out of commands or frames.
evalProgram :: HogoProgram -> State TurtleState ()
evalProgram program = do
  turtle <- get

  if remainingFrames turtle <= 0 || null program then
    pure ()
  else do
    -- Run the command at the head of the list and then call `evalProgram` on the remaining commands
    runCommand $ head program
    evalProgram $ tail program


{-
  Run an individual hogo command.

  The pattern matching is done in the case statement rather than at the top level so that the command handling
  functions don't have to be within State, and don't have to each `get` and `put` the turtle state which would result in
  very repetitive code.
-}
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

  -- call the command function with the turtle state, and update the current state with the state returned
  put $ f turtle


-- | Drawing Commands

{-
  Move the turtle forwards.

  - Move the turtle forwards as far as it can go given the number of frames remaining.
  - Draw a line if the pen is down.
  - Add a vertex to the currently drawing polygon if filling is currently on.
-}
forwardCommand :: Expression -> TurtleState -> TurtleState
forwardCommand distance turtle = turtle {
    position = newPosition,
    linesDrawnSoFar = newLines,
    remainingFrames = newRemainingFrames,
    currentPolygonDrawing = newCurrentPolygonDrawing
  }
  where
    -- Evaluate the expression representing the maximum forward movement
    dist = evaluate distance (symbolTable turtle)

    -- Get the remaining number of frames
    -- Calculate the number of frames that should be remaining after moving, not letting it go negative.
    -- Calculate the distance the turtle will move over this time
    currentRemainingFrames = remainingFrames turtle
    newRemainingFrames = max 0 $ currentRemainingFrames - (abs dist / speed turtle)
    distCompletion = signum dist * (currentRemainingFrames - newRemainingFrames) * speed turtle

    (x, y) = position turtle
    theta = pi / 180 * angle turtle

    newX = x + distCompletion * sin theta
    newY = y + distCompletion * cos theta
    newPosition = (newX, newY)

    -- Add new line if pen is down
    currentLines = linesDrawnSoFar turtle
    newLines
      | penDown turtle = ((x, y), (newX, newY), colour turtle) : currentLines
      | otherwise      = currentLines

    -- Add new vertex to polygon if currently filling
    newCurrentPolygonDrawing
      | filling turtle = (newX, newY) : currentPolygonDrawing turtle
      | otherwise      = currentPolygonDrawing turtle

-- Backwards is just forwards with a negative argument
backwardCommand :: Expression -> TurtleState -> TurtleState
backwardCommand dist = forwardCommand (Negate dist)


-- | Turning Commands
turnLeftCommand  :: Expression -> TurtleState -> TurtleState
turnRightCommand :: Expression -> TurtleState -> TurtleState

-- Simply calculate and set the new angle
turnLeftCommand  dTheta turtle = turtle { angle = angle turtle - evaluate dTheta (symbolTable turtle) }
turnRightCommand dTheta turtle = turtle { angle = angle turtle + evaluate dTheta (symbolTable turtle) }

-- Set the speed field in the turtle state
setSpeedCommand :: Expression -> TurtleState -> TurtleState
setSpeedCommand newSpeed turtle = turtle { speed = evaluate newSpeed (symbolTable turtle) }

{-
  Wait roughly `duration` number of seconds by decrementing the number of remaining frames by `duration * 30` because
  Hatch is set to run at 30fps
-}
waitCommand :: Expression -> TurtleState -> TurtleState
waitCommand duration turtle = turtle { remainingFrames = max 0 (remainingFrames turtle - (evaluate duration (symbolTable turtle) * 30)) }

-- Set the position and angle to their default values
goHomeCommand :: TurtleState -> TurtleState
goHomeCommand turtle = turtle { position = (0, 0), angle = 0 }


-- | Pen Commands
penUpCommand       :: TurtleState -> TurtleState
penDownCommand     :: TurtleState -> TurtleState
colourCommand      :: Expression  -> Expression -> Expression -> TurtleState -> TurtleState
clearScreenCommand :: TurtleState -> TurtleState
startFillCommand   :: TurtleState -> TurtleState
endFillCommand     :: TurtleState -> TurtleState

-- Simply setting the fields to represent the altered state of the pen
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


-- | Control Flow Commands


{-
  To execute the repeat command, the command block is executed until there are no frames left, or `n` repetitions have
  occurred.

  This is done by calling itself with `n` decremented and the next state computed by calling `execState` on
  `evalProgram` with the command block and current turtle state.
-}
repeatCommand :: Expression -> HogoProgram -> TurtleState -> TurtleState
repeatCommand nExpr commands turtle
  | n <= 0 || remainingFrames turtle <= 0 = turtle
  | otherwise = repeatCommand nMinusOne commands $ execState (evalProgram commands) turtle
  where
    n = evaluate nExpr (symbolTable turtle)
    nMinusOne = Raw (n - 1)

-- Forever is implemented in a very similar way to repeat, except it only stops when there are no frames remaining
foreverCommand :: HogoProgram -> TurtleState -> TurtleState
foreverCommand commands turtle
  | remainingFrames turtle <= 0 = turtle
  | otherwise = foreverCommand commands $ execState (evalProgram commands) turtle


-- | User Defined Constructs

-- Insert/update a variable in the symbol table
assignmentStatement :: String -> Expression -> TurtleState -> TurtleState
assignmentStatement name value turtle = turtle { symbolTable = Map.insert name val $ symbolTable turtle  }
  where val = evaluate value (symbolTable turtle)

-- Insert a subroutine into the subroutine table
subroutineDefinition :: String -> HogoProgram -> TurtleState -> TurtleState
subroutineDefinition name commands turtle = turtle { subroutineTable = Map.insert name commands $ subroutineTable turtle }

-- Run a subroutine. If the subroutine can't be found, nothing happens
subroutineCall :: String -> TurtleState -> TurtleState
subroutineCall name turtle = execState (evalProgram commands) turtle
  where
    subs = subroutineTable turtle
    commands
      | Map.member name subs = subs Map.! name
      | otherwise            = []



-- | Expression Evaluation
evaluate :: Expression -> Map String Float -> Float
evaluate (Raw val)              _    = val
evaluate (Variable name)        vars = if Map.member name vars then vars Map.! name else 0 -- Undefined variable equals zero
evaluate (Negate expr)          v    = -(evaluate expr v)
evaluate (Id expr)              v    = evaluate expr v
evaluate (Exponent expr1 expr2) v    = evaluate expr1 v ** evaluate expr2 v
evaluate (Multiply expr1 expr2) v    = evaluate expr1 v * evaluate expr2 v
evaluate (Divide expr1 expr2)   v    = evaluate expr1 v / evaluate expr2 v
evaluate (Modulo expr1 expr2)   v    = evaluate expr1 v `mod'` evaluate expr2 v
evaluate (Plus expr1 expr2)     v    = evaluate expr1 v + evaluate expr2 v
evaluate (Minus expr1 expr2)    v    = evaluate expr1 v - evaluate expr2 v
