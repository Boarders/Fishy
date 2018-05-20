{-# LANGUAGE RecordWildCards #-}
module Utility where

import Prelude hiding (cycle)
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Semigroup ((<>))


type Image = Diagram B

blank :: Image
blank = mempty

rot :: Image -> Image
rot = \img -> img # rotateAround (P (V2 0.5 0.5)) (pi/2 @@ rad)

flip :: Image -> Image
flip = undefined

above :: Int -> Int -> Image -> Image -> Image
above = undefined

below :: Int -> Int -> Image -> Image -> Image
below = undefined

quartet :: Image -> Image
        -> Image -> Image       -> Image
quartet p q
        r s =
                 (p
               <> q # translate unit_Y
               <> r # translate (unitX + unit_Y)
               <> s # translate unitX
                  )
                   # translate unitY
                   # scale 0.5

cycle :: Image -> Image
cycle p = quartet p (rot p) (rot . rot $ p) (rot . rot . rot $ p)

nonet :: Image -> Image -> Image
      -> Image -> Image -> Image
      -> Image -> Image -> Image     -> Image
nonet     p         q        r
          s         t        u
          v         w        x =
          p'  <>    q'  <>   r'
       <> s'  <>    t'  <>   u'
       <> v'  <>    w'  <>   x'
  where  -- CURSED WHERE CLAUSE
         p' , q' , r' , s' , t' , u' , v' , w' , x' :: Image
         p' = p # scale (1 / 3) # translate ((2 / 3) * unitY +  0      * unitX)
         q' = q # scale (1 / 3) # translate ((2 / 3) * unitY + (1 / 3) * unitX)
         r' = r # scale (1 / 3) # translate ((2 / 3) * unitY + (2 / 3) * unitX)
         s' = s # scale (1 / 3) # translate ((1 / 3) * unitY +  0      * unitX)
         t' = t # scale (1 / 3) # translate ((1 / 3) * unitY + (1 / 3) * unitX)
         u' = u # scale (1 / 3) # translate ((1 / 3) * unitY + (2 / 3) * unitX)
         v' = v # scale (1 / 3) # translate ( 0      * unitY +  0      * unitX)
         w' = w # scale (1 / 3) # translate ( 0      * unitY + (1 / 3) * unitX)
         x' = x # scale (1 / 3) # translate ( 0      * unitY + (2 / 3) * unitX)
