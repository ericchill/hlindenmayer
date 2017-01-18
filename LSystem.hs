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

data LSystem a = LSystem {
  lOptions     :: OptionMap,
  lMacros      :: (Turt a) => ActionMap a,
  lGrammar     :: Grammar,
  lAxiom       :: String
  }

type LSystemError a = ErrorM (LSystem a)

emptySystem :: (Turt a) => Metagrammar -> LSystem a
emptySystem meta = LSystem Map.empty Map.empty (newGrammar meta) []

addFloatOption :: (Turt a) => LSystem a -> String -> String -> LSystemError a
addFloatOption sys k v = do
  f <- mapErrorM $ readM v `amendE'` ("Adding float option " ++ k)
  return sys { lOptions = Map.insert k (FloatOpt f) $ lOptions sys }

addOption :: (Turt a) => LSystem a -> String -> String -> LSystem a
addOption sys k v = sys { lOptions = Map.insert k (StringOpt v) $ lOptions sys }

getMacros :: (Turt a) => LSystem a -> ActionMap a
getMacros = lMacros

getOptions :: LSystem a -> OptionMap
getOptions = lOptions

addMacro :: (Turt a) => LSystem a -> String -> String -> LSystemError a
addMacro sys k v = do
  actions <- appendErrorT ("addMacro: " ++ k ++ "=" ++ show v) (encodeActions v)
  return $ sys { lMacros = Map.insert k actions $ lMacros sys }

setAxiom :: (Turt a) => LSystem a -> String -> LSystem a
setAxiom sys a = sys { lAxiom  = a }

setGrammar :: (Turt a) => LSystem a -> Grammar -> LSystem a
setGrammar sys g = sys { lGrammar = g }

setIgnore :: (Turt a) => LSystem a -> String -> LSystem a
setIgnore sys x = sys { lGrammar = gSetIgnore x $ lGrammar sys }
