module POVTurtle (
  POVTurtle(..),
  povLSystem,
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

data POVTurtle = POVTurtle {
  tPos        :: TPosition,
  tOrient     :: TOrientation,  -- tOrient_x is speed
  tAngle      :: Double,
  tInPoly     :: Bool,
  tPolyPoints :: [V3F],
  tPen        :: Double,
  tPenScale   :: Double,
  tColor      :: String,
  tTexture    :: String,
  tMacros     :: ActionMap POVTurtle,
  tOptions    :: OptionMap
  }

povLSystem :: LSystem POVTurtle -> String -> ErrorIO ()
povLSystem sys lString =
  let options = getOptions sys
      macros  = getMacros sys
   in do
    turtle <- mapErrorM $ povTurtle macros options
    actions <- mapErrorM $ encodeActions lString
    foldActions actions turtle
    return ()

povTurtle :: ActionMap POVTurtle -> OptionMap -> ErrorM POVTurtle
povTurtle macros options = do
  angle <- getFloatOption "delta" 90.0 options
  penScale <- getFloatOption "pen_scale" 1.1 options
  color <- getStringOption "color" "" options
  texture <- getStringOption "texture" "" options
  return POVTurtle {
    tPos = V3 0 0 0,
    tOrient = initialOrientation,
    tAngle = angle,
    tInPoly = False,
    tPolyPoints = [],
    tPen = 1,
    tPenScale = penScale,
    tColor = color,
    tTexture = texture,
    tMacros = macros,
    tOptions = options
    }
  
showLine :: POVTurtle -> V3F -> V3F -> String
showLine t p1 p2 =
  let fromStr = showV3 p1
      toStr = showV3 p2
  in
    if fromStr /= toStr then
      "cylinder{" ++ showV3 p1 ++ "," ++ showV3 p2 ++
      "," ++ showFloat (0.5 * tPen t) ++
      showTexture t ++ "}"
    else ""

showSphere :: POVTurtle -> String
showSphere t =
  "sphere{" ++ showV3 (tPos t) ++
  ", " ++ show (tPen t / 2.0) ++
  showTexture t ++ "}"

showTexture :: POVTurtle -> String
showTexture t =
  if tTexture t /= "" then
    " texture{" ++ tTexture t ++ "}"
  else
    ""

instance Turt POVTurtle where
  drawLine t arg = do
    moved <- move t arg
    (liftIO . putStrLn) $ showLine t (tPos t) (tPos moved)
    return moved

  drawNoMark = drawLine

  drawSphere t =
    let at = tPos t
        diam = max 0.1 $ tPen t
    in do
      (liftIO . putStrLn) $ showSphere t
      return t

  move t arg = do
    dist <- mapErrorM $ getFloatArg arg t
    return $ t { tPos = translateX (tOrient t) dist $ tPos t }

  getPos = tPos
  
  setPos t x = return $ t { tPos = x }
  
  getOrientation = tOrient
  
  setOrientation t o = return $ t { tOrient = o }
    
  resetOrientation t = return $ t { tOrient = initialOrientation }

  startPolygon t =
    if tInPoly t then return t  -- igore
    else return $ t { tInPoly = True, tPolyPoints = [] }
    
  markVertex t =
    if tInPoly t then
      return $ t { tPolyPoints = tPos t : tPolyPoints t }
    else
      throwError "Trying to mark vertex while turtle not in polygon."

  endPolygon t =
    if tInPoly t then
      return t -- For now
    else
      throwError "Trying to complete polygon that hasn't been started."
    

  getPenWidth = tPen
  
  setPenWidth t arg = do
    width <- mapErrorM $ getFloatArg arg t
    return $ t { tPen = width }

  getPenScale = return . tPenScale
  
  setPenScale t arg = do
    s <- mapErrorM $ getFloatArg arg t
    return $ t { tPenScale = s }

  getColor = return . tColor

  setColor t arg = do
    c <- mapErrorM $ getStringArg arg t
    return $ t { tColor = c }

  getTexture = return . tTexture
    
  setTexture t arg = do
    texture <- mapErrorM $ getStringArg arg t
    return $ t { tTexture = texture }

  getMacro t arg =
    getStringArg arg t >>= (\val ->
    return $ fromMaybe [] $ Map.lookup val $ tMacros t)

  getAngle t = return $ tAngle t * pi / 180.0

  setAngle t arg = do
    a <- mapErrorM $ getFloatArg arg t
    return $ t { tAngle = a }

  getFloatOpt t key def = getFloatOption key def $ tOptions t

  getStringOpt t key def = getStringOption key def $ tOptions t

initialOrientation :: M33F
initialOrientation =
  V3 (V3 0 1 0)
     (V3 1 0 0)
     (V3 0 0 1)
