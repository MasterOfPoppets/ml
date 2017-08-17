module Main where

import Lib

calculatePrice :: (Float, Float, Float) -> Float -> Float -> Float
calculatePrice (x, y, z) width height = (width * x) + (height * y) + z

main :: IO ()
main = do
  results <-
    basicExample
      [0.900, 1.0, 1.0, 1.130, 1.049, 0.782, 1.182]
      [2.100, 2.100, 1.950, 2.100, 2.034, 2.234, 1.987]
      [1011.0, 1119.0, 1044.0, 1259.0, 1137.28, 936.11, 1248.67]
  print $ results
  print "1248.67"
  print $ calculatePrice results 1.182 1.987
