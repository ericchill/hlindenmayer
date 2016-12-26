module PlotTurtle (
  PlotTurtle(..),
  plotLSystem,
  Turt
  ) where
import Math
import Grammar
import Turtle
import Parse
import Pen
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Linear.V3

plotLSystem :: LSystem PlotTurtle Char -> String -> IO PlotTurtle
plotLSystem sys lString =
  let turtle = plotTurtle (getFloatOption sys "delta" (pi/4)) $ lMacros sys
  in
    case encodeActions lString of
      Right actions -> foldActions turtle actions
      Left err ->
        do
          print err
          return turtle
    
plotTurtle :: Float -> Map.Map String [TAction PlotTurtle] -> PlotTurtle
plotTurtle angle =
  PlotTurtle (V3 0 0 0) initialOrientation angle (Pen 1)
  
data PlotTurtle = PlotTurtle {
  tPos    :: TPosition,
  tOrient :: TOrientation,  -- tOrient_x is speed
  tAngle  :: Float,
  tPen    :: Pen,
  tMacros :: Map.Map String [TAction PlotTurtle]
  }

showLine :: V3F -> V3F -> String
showLine from to = "(" ++ show from ++ ", " ++ show to ++ ")"

instance Turt PlotTurtle where
  drawLine turtle arg =
    let from = tPos turtle in
      do
        moved <- move turtle arg
        putStrLn $ showLine from $ tPos moved
        return moved

  drawNoMark = drawLine

  move tur@(PlotTurtle pos orient angle pen mac) (FloatArg arg) =
    return $ PlotTurtle (translateX orient (arg tur) pos) orient angle pen mac

  getPos = tPos
  
  setPos (PlotTurtle _ orient angle pen mac) pos =
    return $ PlotTurtle pos orient angle pen mac
  
  getOrientation = tOrient
  
  setOrientation (PlotTurtle pos _ angle pen mac) orient =
    return $ PlotTurtle pos orient angle pen mac
    
  resetOrientation (PlotTurtle pos _ angle pen mac) =
    return $ PlotTurtle pos initialOrientation angle pen mac

  getPenWidth tur = pWidth $ tPen tur
  
  setPenWidth tur@(PlotTurtle pos orient angle _ mac) (FloatArg arg) =
    return $ PlotTurtle pos orient angle (Pen $ arg tur) mac
  
  getMacro turtle (StringArg arg) =
    fromMaybe [] $ Map.lookup (arg turtle) $ tMacros turtle

initialOrientation :: M33F
initialOrientation =
  V3 (V3 0 1 0)
     (V3 1 0 0)
     (V3 0 0 1)
