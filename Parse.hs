module Parse (
  LSystem(..),
  parseRuleFile,
  getOption,
  getFloatOption,
  getIntOption,
  module Grammar,
  )
where
import Debug.Trace
import Metagrammar
import Grammar
import RuleSpec
import Turtle (TAction, encodeActions, Turt)
import Prelude hiding (lookup, null)
import Control.Monad
import Data.Char (isSpace)
import Data.List (foldl', isPrefixOf, null, stripPrefix)
import Data.Maybe (fromJust, fromMaybe)
import Data.String.Utils
import Data.Map.Strict (Map, empty, insert, lookup)

{-
  rule
    | [stuff <] word [> stuff] --> words
  stuff
    | *
    | words
-}

metagrammar :: String -> Metagrammar Char
metagrammar toIgnore = Metagrammar
  (`elem` "([")
  (`elem` ")]")
  (\a b -> case a of
      '(' -> b == ')'
      '[' -> b == ']'
      _   -> False)
  isSpace
  (`elem` toIgnore)
  (== '*')
  (== '%')
  "()[]*"

data LSystem a b = LSystem {
  lOptions     :: Map String String,
  lMacros      :: Map String [TAction a],
  lGrammar     :: Grammar b,
  lAxiom       :: [b]
  } -- deriving (Show)

parseRuleFile :: Turt a => String -> Either String (LSystem a Char)
parseRuleFile text =
  foldM addLineToSystem emptySystem $ filterComments $ lines text

addOption :: Turt a => String -> String -> LSystem a b -> LSystem a b
addOption k v (LSystem o m g a) = LSystem (insert k v o) m g a

getOption :: Turt a => LSystem a b -> String -> String -> String
getOption sys key dflt =
  fromMaybe dflt $ lookup key $ lOptions sys

getFloatOption :: Turt a => LSystem a b -> String -> Float -> Float
getFloatOption sys key dflt =
  case lookup key $ lOptions sys of
    Just value -> read value
    Nothing    -> dflt

getIntOption :: (Turt a, Ord b) => LSystem a b -> String -> Int -> Int
getIntOption sys key dflt =
  case lookup key $ lOptions sys of
    Just value -> read value
    Nothing    -> dflt

addMacro :: Turt a => String -> String -> LSystem a b -> Either String (LSystem a b)
addMacro k v (LSystem o m g a) =
  case encodeActions v of
    Right actions -> Right $ LSystem o (insert k actions m) g a
    Left err -> Left err

setAxiom :: (Turt a, Show b) => [b] -> LSystem a b -> LSystem a b
setAxiom a (LSystem opt mac gram _) = LSystem opt mac gram a

setGrammar :: Turt a => Grammar b -> LSystem a b -> LSystem a b
setGrammar a (LSystem opt mac _ ax) = LSystem opt mac a ax

emptySystem :: Turt a => LSystem a Char
emptySystem = LSystem empty empty (newGrammar $ metagrammar "") ""

filterComments :: [String] -> [String]
filterComments ls =
  fst $ foldr (\l (res, incomment) ->
                 if incomment then
                   if "{-" `isPrefixOf` l then (res, False) else (res, True)
                 else
                   if "-}" `isPrefixOf` l then (res, True) else (l : res, False)
              ) ([], False) $
  filter (not . ("--" `isPrefixOf`)) $
  filter (not.null) $ map strip ls

addLineToSystem :: Turt a => LSystem a Char -> String -> Either String (LSystem a Char)
addLineToSystem sys [] = Right sys
addLineToSystem sys line
  | ':' `elem` line = addParam line sys
  | otherwise       = addRule line sys

addParam :: Turt a => String -> LSystem a Char -> Either String (LSystem a Char)
addParam line sys
  | "axiom:" `isPrefixOf` line =
    Right $ setAxiom (argFor "axiom:") sys
  | "define:" `isPrefixOf` line =
    let (name, def) = break isSpace $ argFor "define:"
    in
      addMacro name (strip def) sys
  | "delta:" `isPrefixOf` line =
    Right $ addOption "delta" (argFor "delta:") sys
  | "ignore:" `isPrefixOf` line =
    Right $ setIgnore (argFor "ignore:") sys
  | "iterate:" `isPrefixOf` line =
    Right $ addOption "iterate" (argFor "iterate:") sys
  | "stepRatio:" `isPrefixOf` line =
    Right $ addOption "stepRatio" (argFor "stepRatio:") sys
  | otherwise = Right sys
  where argFor = cleanArgument line

cleanArgument :: String -> String -> String
cleanArgument str argName = strip $ fromJust $ stripPrefix argName str

setIgnore :: Turt a => String -> LSystem a Char -> LSystem a Char
setIgnore toIgnore sys =
  setGrammar (setMetagrammar (metagrammar toIgnore) $ lGrammar sys) sys

addRule :: Turt a => String -> LSystem a Char -> Either String (LSystem a Char)
addRule line sys =
  case parseRule (getMetagrammar $ lGrammar sys) line of
    Just specAndProd -> Right $ setGrammar (addRuleFromSpec specAndProd $ lGrammar sys) sys
    Nothing -> Right sys

parseRule :: Metagrammar Char -> String -> Maybe (RuleSpec Char, String)
parseRule meta input
  | length sides == 2 =
    if null specStr then Nothing
    else Just (parseRuleSpec meta specStr, strip prodStr)
  | otherwise = Nothing
  where sides = split "-->" input
        specStr = head sides
        prodStr = sides !! 1

parseRuleSpec :: Metagrammar Char -> String -> RuleSpec Char
parseRuleSpec meta [] = error "Empty RuleSpec string."
parseRuleSpec meta str
  | 2 == length pieces = parseRuleSpec2 meta ((reverse . head) pieces) $ pieces !! 1
  | otherwise          = parseRuleSpec2 meta "*" $ head pieces
  where pieces = map strip $ split "<" str

parseRuleSpec2 :: Metagrammar Char -> String -> String -> RuleSpec Char
parseRuleSpec2 meta _ [] = error "No RuleSpec after left constraint"
parseRuleSpec2 meta left str
  | 2 == length pieces = case head pieces of
      "" -> error "No center element in RuleSpec."
      center  -> case tail pieces of
        [""]    -> error "Empty right constraint in RuleSpec."
        right   -> RuleSpec meta left center $ head right
  | otherwise = RuleSpec meta left str "*"
  where pieces = map strip $ split ">" str
