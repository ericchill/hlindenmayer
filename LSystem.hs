{-# LANGUAGE RankNTypes #-}
module LSystem (
  LSystem(..),
  LSystemError(..),
  ActionMap,
  emptySystem,
  addOption,
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

addOption :: (Turt a) => String -> String -> LSystem a b -> LSystem a b
addOption k v sys = sys { lOptions = Map.insert k v $ lOptions sys }

getMacros :: (Turt a) => LSystem a b -> ActionMap a
getMacros = lMacros

getOptions :: LSystem a b -> OptionMap
getOptions = lOptions

addMacro :: (Turt a) => String -> String -> LSystem a b -> LSystemError a b
addMacro k v sys = do
  actions <- withExceptT (++ " in addMacro") (encodeActions v)
  return $ sys { lMacros = Map.insert k actions $ lMacros sys }

setAxiom :: (Turt a, Show b) => [b] -> LSystem a b -> LSystem a b
setAxiom a sys = sys { lAxiom  = a }

setGrammar :: (Turt a, Show b) => Grammar b -> LSystem a b -> LSystem a b
setGrammar g sys = sys { lGrammar = g }

setIgnore :: (Turt a, Eq b) => [b] -> LSystem a b -> LSystem a b
setIgnore x sys = sys { lGrammar = gSetIgnore x $ lGrammar sys }
