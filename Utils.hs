module Utils (
  ErrorM(..),
  BoolMonad(..),
  caseM,
  (&&&&), (||||), lNot,
  FloatArg(..),
  floatConst,
  getFloatArg,
  StringArg(..),
  stringConst,
  getStringArg,
  module Error,
  module Control.Applicative,
  )
  where
import Error
import Control.Applicative

{-
> caseM [(m Bool, a)] default
-}
caseM :: [(ErrorM Bool, ErrorM a)] -> ErrorM a -> ErrorM a
caseM [] def = def
caseM (c:cs) def =
  let (cond, action) = c in
    cond >>= (\test ->
                if test then action
                else caseM cs def)

(&&&&) :: (Applicative m) => m Bool -> m Bool -> m Bool
(&&&&) = liftA2 (&&)

(||||) :: (Applicative m) => m Bool -> m Bool -> m Bool
(||||) = liftA2 (||)

lNot :: (Applicative m) => m Bool -> m Bool
lNot = liftA not


data FloatArg a = FloatVar (a -> Float) | FloatConst Float

instance Show (FloatArg a) where
  show (FloatVar _) = "FloatVar"
  show (FloatConst x) = show x

floatConst :: Float -> FloatArg a
floatConst = FloatConst

getFloatArg :: FloatArg a -> a -> Float
getFloatArg (FloatVar f) = f
getFloatArg (FloatConst x) = const x

data StringArg a = StringVar (a -> String) | StringConst String

instance Show (StringArg a) where
  show (StringVar _) = "StringVar"
  show (StringConst s) = s

stringConst :: String -> StringArg a
stringConst = StringConst

getStringArg :: StringArg a -> a -> String
getStringArg (StringVar f) = f
getStringArg (StringConst s) = const s
