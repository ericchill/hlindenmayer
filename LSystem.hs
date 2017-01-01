{-# LANGUAGE RankNTypes #-}
module LSystem (
  LSystem(..),
  LSystemError(..),
  emptySystem,
  addOption,
  getOption,
  addMacro,
  setAxiom,
  setGrammar
  ) where
import Grammar
import Turtle
import Utils
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.String.Utils

data LSystem a b = LSystem {
  lOptions     :: Map.Map String String,
  lMacros      :: (Turt a) => Map.Map String [TAction a],
  lGrammar     :: Grammar b,
  lAxiom       :: [b]
  }

type LSystemError a b = ErrorM (LSystem a b)

emptySystem :: (Turt a) => Metagrammar b -> LSystem a b
emptySystem meta = LSystem Map.empty Map.empty (newGrammar meta) []

addOption :: (Turt a) => String -> String -> LSystem a b -> LSystem a b
addOption k v sys = sys { lOptions = Map.insert k v $ lOptions sys }

getOpt' :: (Turt a) => LSystem a b -> String -> Maybe String
getOpt' sys key = Map.lookup key $ lOptions sys

getOption :: (Turt a, Read b) => LSystem a c -> String -> b -> ErrorM b
getOption sys key dflt =
  case getOpt' sys key of
    Nothing -> return dflt
    Just str -> case maybeRead str of
      Just x -> return x
      otherwise -> throwE $ str ++ " can't be parsed as desired type."

addMacro :: (Turt a) => String -> String -> LSystem a b -> LSystemError a b
addMacro k v sys = do
  actions <- withExceptT (++ " in addMacro") (encodeActions v)
  return $ sys { lMacros = Map.insert k actions $ lMacros sys }

setAxiom :: (Turt a, Show b) => [b] -> LSystem a b -> LSystem a b
setAxiom a sys = sys { lAxiom  = a }

setGrammar :: (Turt a, Show b) => Grammar b -> LSystem a b -> LSystem a b
setGrammar g sys = sys { lGrammar = g }
