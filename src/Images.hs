{-# LANGUAGE LambdaCase #-}
module Images where

import Prelude hiding (cycle)
import Diagrams.Prelude
import Diagrams.Backend.SVG

import Utility
import Fish
import Consts(windowSize, windowPos)

testImage = squareLimit 4

fac = (1 / sqrt 2)

fishy :: Diagram B
fishy = fishOutline aquamarine

fishy2c :: Colour Double -> Diagram B
fishy2c = \col ->
           fishOutline col
           # scale fac
           # (translate $ (1 - fac) * unitY)
           # rotateAround (P (V2 0 1)) (pi/4 @@ rad)
           # reflectX
           # translate unitX
fishy2  = fishy2c lightseagreen
fishy2' = fishy2c lightseagreen


fishy3c = \col -> fishy2c col
          # rotateAround (P (V2 0.5 0.5)) (3 * pi / 2 @@ rad)

fishy3  = fishy3c lightslategray
fishy3' = fishy3c aquamarine

fishy4 :: Diagram B
fishy4 = fishy2 <> fishy3

fishy4' :: Diagram B
fishy4' = fishy2' <> fishy3'

tileT :: Diagram B
tileT = fishy <> fishy4

tileU :: Diagram B
tileU = fishy4 <>
        fishy4' # rotateAround (P (V2 0.5 0.5)) (pi @@ rad)


side :: Int -> Image
side = \case
  0 -> blank
  1 -> quartet blank (rot tileT) tileT blank
  n -> quartet (side (n-1)) (rot tileT) tileT (side (n-1))

corner :: Int -> Image
corner = \case
  0 -> blank
  1 -> quartet blank blank tileU blank
  n -> quartet (corner (n-1)) (rot $ (side n)) tileU (side n)

squareLimit :: Int -> Image
squareLimit n = nonet
  (      corner n) (            side n) (rot . rot . rot $ corner n)
  (rot $ side   n)              tileU   (rot . rot . rot $ side   n)
  (rot $ corner n) (rot . rot $ side n) (rot . rot       $ corner n)
