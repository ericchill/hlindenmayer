module PSTurtle where
import Math
import Turtle

data DrawState = DrawState {
  outputColumn :: Int,
  moving       :: Bool,
  lastPos      :: V3F,
  havePath     :: Bool,
  strokeCount  :: Int,
  penWidth     :: Float
  }

initialDrawState :: DrawState
initialDrawState = DrawState 0 False (V3 0 0 0) False 0 1.0

emitMoveOrDraw :: DrawState -> V3F -> Bool -> Float -> (String, DrawState)
emitMoveOrDraw state dest draw width =
  ("", initialDrawState)

