module Options (
    OptionMap
  , OptionValue(..)
  , FloatArg(..)
  , floatConst
  , getFloatArg
  , StringArg(..)
  , stringConst
  , getStringArg
  , getFloatOption
  , getIntOption
  , getStringOption
  , readM
  , addOptionBindings
  ) where
import Error
import NumEval
import Utils
import qualified Data.Map.Strict as Map
--import Text.Read

{-
TODO - OptionMap should be String -> OptionValue
  where value is FloatArg, StringArg, ColorArg, ...
-}

data OptionValue = StringOpt String | FloatOpt Double | IntOpt Int
       deriving (Show)

type OptionMap = Map.Map String OptionValue

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
  
getIntOption :: String -> Int -> OptionMap -> ErrorM Int
getIntOption k def map =
  case Map.lookup k map of
    Just x -> case x of
      StringOpt str -> readM str
      FloatOpt x    -> return $ round x
      IntOpt x      -> return x
    Nothing  -> return def

getFloatOption :: String -> Double -> OptionMap -> ErrorM Double
getFloatOption k def map =
  case Map.lookup k map of
    Just x -> case x of
      StringOpt str -> readM str
      FloatOpt x    -> return x
      IntOpt x      -> return $ fromIntegral x
    Nothing  -> return def

getStringOption :: String -> String -> OptionMap -> ErrorM String
getStringOption k def map =
  case Map.lookup k map of
    Just x -> case x of
      StringOpt str -> return str
      FloatOpt x    -> return $ show x
      IntOpt x      -> return $ show x
    Nothing  -> return def

addOptionBindings :: OptionMap -> Bindings -> Bindings
addOptionBindings map bindings =
  foldr (\k b ->
           case Map.lookup k map of
             Just x -> case x of
               FloatOpt f -> bindScalar k (Evaluator (\_ -> return f)) b
               IntOpt i   -> bindScalar k (Evaluator (
                                              \_ -> return $ fromIntegral i)) b
               _ -> b
             Nothing -> b) bindings $ Map.keys map

