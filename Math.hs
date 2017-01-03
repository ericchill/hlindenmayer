module Math (
  V3F(..),
  M33F(..),
  showV3,
  xAxis, yAxis, zAxis,
  rotateMatrix,
  translateX,
  module M,
  module V,
  module V3
  )
where
import Utils
import Control.Lens
import Linear.Matrix as M
import Linear.Quaternion
import Linear.V3 as V3
import Linear.Vector as V

type V3F  = V3 Double
type M33F = M33 Double

showV3 :: V3F -> String
showV3 (V3 x y z) = "<" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ">"
    
rotateMatrix :: M33F -> V3F -> Double -> M33F
rotateMatrix mat axis angle =
  --mat !*! fromQuaternion (axisAngle axis (trace (show angle) angle))
  mat !*! fromQuaternion  (axisAngle axis angle)

xAxis :: V3F
xAxis = V3 1 0 0

yAxis :: V3F
yAxis = V3 0 1 0

zAxis :: V3F
zAxis = V3 0 0 1

translateX :: M33F -> Double -> V3F -> V3F
translateX m sx v = v + sx *^ (m ^. column _x)
