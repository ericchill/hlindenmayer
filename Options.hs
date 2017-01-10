module Options (
  OptionMap,
  FloatArg(..),
  floatConst,
  getFloatArg,
  StringArg(..),
  stringConst,
  getStringArg,
  getOption,
  readM,
  ) where
import Error
import Utils
import qualified Data.Map.Strict as Map
--import Text.Read

{-
TODO - OptionMap should be String -> OptionValue
  where value is FloatArg, StringArg, ColorArg, ...
-}
type OptionMap = Map.Map String String

data FloatArg a = FloatVar (a -> ErrorM Double) | FloatConst Double

instance Show (FloatArg a) where
  show (FloatVar _) = "FloatVar"
  show (FloatConst x) = show x

floatConst :: Double -> FloatArg a
floatConst = FloatConst

getFloatArg :: FloatArg a -> a -> ErrorM Double
getFloatArg (FloatVar f) = f
getFloatArg (FloatConst x) = return . const x

data StringArg a = StringVar (a -> ErrorM String) | StringConst String

instance Show (StringArg a) where
  show (StringVar _) = "StringVar"
  show (StringConst s) = s

stringConst :: String -> StringArg a
stringConst = StringConst

getStringArg :: StringArg a -> a -> ErrorM String
getStringArg (StringVar f) = f
getStringArg (StringConst s) = return . const s
  
getOption :: (Read a) => String -> a -> OptionMap -> ErrorM a
getOption k def map =
  case Map.lookup k map of
    Just str -> readM str
    Nothing  -> return def
