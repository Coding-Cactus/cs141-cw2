--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Getting started                                                       --
--------------------------------------------------------------------------------

module Transforms where

--------------------------------------------------------------------------------

import Layout
import qualified Graphics.Gloss as Gloss

import Graphics.Gloss (color)
import Graphics.Gloss.Data.Color

--------------------------------------------------------------------------------

-- Rotate an image in degrees.
-- Wraps the Gloss Picture rotation.
rotate :: Int -> Image -> Image
rotate r = fmap (Gloss.rotate $ fromIntegral r)

offset :: Int -> Int -> Image -> Image
offset x y = fmap $ Gloss.translate (fromIntegral x) (fromIntegral y)

scale :: Float -> Image -> Image
scale s = fmap $ Gloss.scale s s

mirror :: Image -> Image
mirror = fmap $ Gloss.scale (-1) 1

applyColour :: Int -> Int -> Int -> Image -> Image
applyColour r g b = fmap (color rgbColor)
  where rgbColor = makeColorI r g b 255

--------------------------------------------------------------------------------
