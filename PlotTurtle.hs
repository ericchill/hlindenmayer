module PlotTurtle (
  PlotTurtle(..),
  plotLSystem,
  Turt
  ) where
import Error
import Math
import Grammar
import LSystem
import Options
import Parse
import Turtle
import Utils
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Linear.V3

data PlotTurtle = PlotTurtle {
  tPos     :: TPosition,
  tOrient  :: TOrientation,  -- tOrient_x is speed
  tPen     :: Double,
  tMacros  :: ActionMap PlotTurtle,
  tOptions :: OptionMap
  }

plotLSystem :: LSystem PlotTurtle Char -> String -> ErrorIO ()
plotLSystem sys lString =
  let options = getOptions sys
      macros  = getMacros sys
      turtle = plotTurtle macros options
   in do
    actions <- mapErrorM $ encodeActions lString
    foldActions actions turtle
    return ()
    
plotTurtle :: ActionMap PlotTurtle -> OptionMap -> PlotTurtle
plotTurtle = PlotTurtle (V3 0 0 0) initialOrientation 1
  
showLine :: V3F -> V3F -> Double -> String
showLine (V3 x1 y1 _) (V3 x2 y2 _) p =
  show x1 ++ " " ++ show y1 ++ "\n" ++ show x2 ++ " " ++ show y2 ++ "\n"

instance Turt PlotTurtle where
  drawLine t arg =
    let from = tPos t
        pen = max 0.1 $ tPen t
    in do
      moved <- move t arg
      (liftIO . putStrLn) $ showLine from (tPos moved) pen
      return moved

  drawNoMark = drawLine

  move t arg = do
    dist <- mapErrorM $ getFloatArg arg t
    return $ t { tPos = translateX (tOrient t) dist $ tPos t }

  getPos = tPos
  
  setPos t x = return t { tPos = x }
  
  getOrientation = tOrient
  
  setOrientation t o = return t { tOrient = o }
    
  resetOrientation t = return t { tOrient = initialOrientation }

  getPenWidth = tPen
  
  setPenWidth t arg = do
    width <- mapErrorM $ getFloatArg arg t
    return $ t { tPen = width }
  
  setTexture :: a -> StringArg a -> TurtleMonad a
  setTexture t _ = return t

  getMacro t arg = do
    val <- getStringArg arg t
    return $ fromMaybe [] $ Map.lookup val $ tMacros t

  getOpt t key def =
    case Map.lookup key $ tOptions t of
      Just str -> readM str
      Nothing  -> return def

-- Return an initial orientation matrix suitable for a plant.
initialOrientation :: M33F
initialOrientation =
  V3 (V3 0 1 0)
     (V3 1 0 0)
     (V3 0 0 1)
