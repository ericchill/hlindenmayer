{-# LANGUAGE RankNTypes #-}
module LSystem (
  LSystem(..),
  LSystemError(..),
  ActionMap,
  emptySystem,
  addOption,
  addFloatOption,
  getOptions,
  addMacro,
  getMacros,
  setAxiom,
  setGrammar,
  setIgnore
  ) where
import Grammar
import Options
import Turtle
import Utils
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.String.Utils

type ActionMap a = Map.Map String [TAction a]
type StringMap = Map.Map String String

data LSystem a b = LSystem {
  lOptions     :: OptionMap,
  lMacros      :: (Turt a) => ActionMap a,
  lGrammar     :: Grammar b,
  lAxiom       :: [b]
  }

type LSystemError a b = ErrorM (LSystem a b)

emptySystem :: (Turt a) => Metagrammar b -> LSystem a b
emptySystem meta = LSystem Map.empty Map.empty (newGrammar meta) []

addFloatOption :: (Turt a) => LSystem a b -> String -> String -> LSystemError a b
addFloatOption sys k v = do
  f <- mapErrorM $ readM v `amendE'` ("Adding float option " ++ k)
  return sys { lOptions = Map.insert k (FloatOpt f) $ lOptions sys }

addOption :: (Turt a) => LSystem a b -> String -> String -> LSystem a b
addOption sys k v = sys { lOptions = Map.insert k (StringOpt v) $ lOptions sys }

getMacros :: (Turt a) => LSystem a b -> ActionMap a
getMacros = lMacros

getOptions :: LSystem a b -> OptionMap
getOptions = lOptions

addMacro :: (Turt a) => LSystem a b -> String -> String -> LSystemError a b
addMacro sys k v = do
  actions <- appendErrorT "addMacro" (encodeActions v 0.0)
  return $ sys { lMacros = Map.insert k actions $ lMacros sys }

setAxiom :: (Turt a, Show b) => LSystem a b -> [b] -> LSystem a b
setAxiom sys a = sys { lAxiom  = a }

setGrammar :: (Turt a, Show b) => LSystem a b -> Grammar b -> LSystem a b
setGrammar sys g = sys { lGrammar = g }

setIgnore :: (Turt a, Eq b) => LSystem a b -> [b] -> LSystem a b
setIgnore sys x = sys { lGrammar = gSetIgnore x $ lGrammar sys }
