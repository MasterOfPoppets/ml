module Lib where

import Control.Monad (replicateM_)

import qualified TensorFlow.Core as TF
import qualified TensorFlow.GenOps.Core as TF
import qualified TensorFlow.Minimize as TF
import qualified TensorFlow.Ops as TF hiding (initializedVariable)
import qualified TensorFlow.Variable as TF

basicExample :: [Float] -> [Float] -> [Float] -> IO (Float, Float, Float)
basicExample xData yData zData =
  TF.runSession $
  -- Create tensorflow constants for x and y.
  do let x = TF.vector xData
         y = TF.vector yData
         z = TF.vector zData
     -- Create scalar variables for slope and intercept.
     v <- TF.initializedVariable 0
     w <- TF.initializedVariable 0
     b <- TF.initializedVariable 0
     -- Define the loss function.
     let yHat =
           (x `TF.mul` TF.readValue v) `TF.add` (y `TF.mul` TF.readValue w) `TF.add`
           TF.readValue b
         loss = TF.square (yHat `TF.sub` z)
     -- Optimize with gradient descent.
     trainStep <- TF.minimizeWith (TF.gradientDescent 0.001) loss [v, w, b]
     replicateM_ 10000 (TF.run trainStep)
     -- Return the learned parameters.
     (TF.Scalar v', TF.Scalar w', TF.Scalar b') <-
       TF.run (TF.readValue v, TF.readValue w, TF.readValue b)
     return (v', w', b')
