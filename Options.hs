module Options (
  OptionMap,
  FloatArg(..),
  floatConst,
  getFloatArg,
  StringArg(..),
  stringConst,
  getStringArg,
  getOption,
  readM
  ) where
import Error
import qualified Data.Map.Strict as Map
import Data.String.Utils (maybeRead)

type OptionMap = Map.Map String String

data FloatArg a = FloatVar (a -> ErrorM Float) | FloatConst Float

instance Show (FloatArg a) where
  show (FloatVar _) = "FloatVar"
  show (FloatConst x) = show x

floatConst :: Float -> FloatArg a
floatConst = FloatConst

getFloatArg :: FloatArg a -> a -> ErrorM Float
getFloatArg (FloatVar f) = f
getFloatArg (FloatConst x) = return . const x

data StringArg a = StringVar (a -> String) | StringConst String

instance Show (StringArg a) where
  show (StringVar _) = "StringVar"
  show (StringConst s) = s

stringConst :: String -> StringArg a
stringConst = StringConst

getStringArg :: StringArg a -> a -> String
getStringArg (StringVar f) = f
getStringArg (StringConst s) = const s
  
getOption :: (Read a) => String -> a -> OptionMap -> ErrorM a
getOption k def map =
  case Map.lookup k map of
    Just str -> readM str
    Nothing  -> return def

readM :: (Read a) => String -> ErrorM a
readM a =
  case maybeRead a of
    Just b  -> return b
    Nothing -> throwE (a ++ " can't be parsed as desired type.")
