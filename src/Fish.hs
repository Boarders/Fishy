module Fish where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Semigroup ((<>))

import Utility
import Consts

-- Height of big and small triangle
bigH = (1 / 6)
litH = (1 / 24)
bigH' = bigH * (1 / 2)
litH' = litH * (1 / 2)

bottomVerts = zip (repeat (-0.25)) [litH, -litH, bigH, -bigH]
leftVerts   = zip [-bigH, bigH, -litH, litH] (repeat 0.25)
diagVerts   = zipWith (+:)
              (repeat $ scaleV (0.125) (1, -1))
              (zipWith scaleV
                 [  litH' ,    litH',  bigH'  ,  bigH'
                 ,  bigH' ,    bigH',  litH'  ,  litH'
                 ]
                 [ ( 1, 1),  (-1,-1), ( 1, 1) , (-1,-1)
                 , (-1,-1),  ( 1, 1), (-1,-1) , ( 1, 1)
                 ]
              )
allVerts = bottomVerts <> leftVerts <> diagVerts
allVerts' = init allVerts


(+:) :: (Double, Double) -> (Double, Double) -> (Double, Double)
(v, w) +: (v', w') = (v + v', w + w')

scaleV :: Double -> (Double, Double) -> (Double, Double)
scaleV c (v , w) = (c * v, c * w)


fishTrail :: Trail' Line V2 Double
fishTrail = fromOffsets $ map r2 allVerts'

fishOutline' ::  Colour Double -> Diagram B
fishOutline'  col =
  (arc (direction $ r2 (-0.8, 0.8)) ((pi  * (1 / 14)) @@ rad ) ) <>       -- mouth
  (circle 0.01 # fc black # translate (r2 (-0.9, 0.7)))          <>       -- eye
  (fishTrail # closeLine # strokeLoop # fc col)

fishOutline = \col -> fishOutline' col # translate unitX

rightTri :: Diagram B
rightTri  = (fromOffsets [unitX, (unitY + unit_X)]) #closeLine #strokeLoop
