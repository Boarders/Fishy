{-# LANGUAGE LambdaCase #-}
module Main where

import Prelude hiding (cycle)
import Diagrams.Prelude
import Diagrams.Backend.SVG

import Utility
import Fish
import Consts(windowSize, windowPos)
import Images

main :: IO ()
main = renderD

outputFile :: FilePath
outputFile = "test.svg"

dimensions :: SVGFloat n => SizeSpec V2 n
dimensions = mkSizeSpec  $ V2 (Just 400) (Just 400)

renderD = do renderSVG outputFile dimensions $ bg white $
               testImage # clipBy (square 0.96 # translate (V2 0.5 0.5))
