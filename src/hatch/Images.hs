--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Getting started                                                       --
--------------------------------------------------------------------------------

module Images where

--------------------------------------------------------------------------------

import qualified Graphics.Gloss as Gloss
import Graphics.Gloss.Juicy ( loadJuicyPNG )
import System.IO.Unsafe ( unsafePerformIO )
import Layout
import Transforms

import Data.Maybe (fromMaybe)
import Graphics.Gloss (Picture, color)
import Graphics.Gloss.Data.Color

--------------------------------------------------------------------------------

loadPNG :: String -> Image
loadPNG name = scale 0.1 $ Leaf $ png $ "assets/" ++ name ++ ".png"

png :: FilePath -> Picture
png fname = fromMaybe (Gloss.text "PNG ERROR")
            $ unsafePerformIO
            $ loadJuicyPNG fname

ant :: Image
ant = loadPNG "ant"

blank :: Image
blank = Leaf Gloss.blank

rect :: Int -> Int -> Image
rect x y = Leaf $ Gloss.rectangleSolid (fromIntegral x) (fromIntegral y)


line :: Float -> Float -> Float -> Float -> Image
line x1 y1 x2 y2 = Leaf $ Gloss.line [(x1, y1), (x2, y2)]

polygon :: [(Float, Float)] -> Image
polygon = Leaf . Gloss.polygon

applyColour :: Int -> Int -> Int -> Image -> Image
applyColour r g b = fmap (color rgbColor)
  where rgbColor = makeColorI r g b 255

text :: String -> Image
text t = Leaf $ Gloss.pictures [
            Gloss.translate x y
                $ Gloss.color Gloss.white
                $ Gloss.scale 0.5 0.5
                $ Gloss.text t
            | x <- [-2..2]
            , y <- [-2..2]
        ]

--------------------------------------------------------------------------------