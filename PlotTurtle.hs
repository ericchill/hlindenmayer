module PlotTurtle (
  PlotTurtle(..),
  plotLSystem,
  Turt
  ) where
import Math
import Grammar
import Turtle
import Parse
import Utils
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Linear.V3

data PlotTurtle = PlotTurtle {
  tPos    :: TPosition,
  tOrient :: TOrientation,  -- tOrient_x is speed
  tAngle  :: Float,
  tPen    :: Float,
  tMacros :: Map.Map String [TAction PlotTurtle]
  }

plotLSystem :: LSystem PlotTurtle Char -> String -> ExceptT String IO () -- ErrorM (IO ())
plotLSystem sys lString =
  do
    let note = " in plotLSystem"
    turnAngle <- mapErrorM (getOption sys "delta" 48 :: ErrorM Float)
    let turtle = plotTurtle (pi / 180 * turnAngle) $ lMacros sys
    actions <- mapErrorM (encodeActions lString)
    foldActions actions turtle
    return ()
    
plotTurtle :: Float -> Map.Map String [TAction PlotTurtle] -> PlotTurtle
plotTurtle angle =
  PlotTurtle (V3 0 0 0) initialOrientation angle 1
  
showLine :: V3F -> V3F -> String
showLine (V3 x1 y1 _) (V3 x2 y2 _) =
  show x1 ++ " " ++ show y1 ++ "\n" ++ show x2 ++ " " ++ show y2

instance Turt PlotTurtle where
  drawLine turtle arg =
    let from = tPos turtle in
      do
        moved <- move turtle arg
        (liftIO . putStrLn) $ showLine from $ tPos moved
        (liftIO . putStrLn) ""
        return moved

  drawNoMark = drawLine

  move tur@(PlotTurtle pos orient angle pen mac) arg =
    return $ PlotTurtle (translateX orient (getFloatArg arg tur) pos) orient angle pen mac

  getPos = tPos
  
  setPos (PlotTurtle _ orient angle pen mac) pos =
    return $ PlotTurtle pos orient angle pen mac
  
  getOrientation = tOrient
  
  setOrientation (PlotTurtle pos _ angle pen mac) orient =
    return $ PlotTurtle pos orient angle pen mac
    
  resetOrientation (PlotTurtle pos _ angle pen mac) =
    return $ PlotTurtle pos initialOrientation angle pen mac

  getPenWidth = tPen
  
  setPenWidth tur@(PlotTurtle pos orient angle _ mac) arg =
    return $ PlotTurtle pos orient angle (getFloatArg arg tur) mac
  
  getMacro turtle arg =
    fromMaybe [] $ Map.lookup (getStringArg arg turtle) $ tMacros turtle

initialOrientation :: M33F
initialOrientation =
  V3 (V3 0 1 0)
     (V3 1 0 0)
     (V3 0 0 1)
