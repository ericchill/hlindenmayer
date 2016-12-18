module Parse (
  LSystem(..),
  parseRuleFile,
  Grammar
  )
where
import Debug.Trace
import Metagrammar
import Grammar
import RuleSpec
import Prelude hiding (null)
--import Control.Monad
import Data.Char (isSpace)
import Data.List (foldl', isPrefixOf, null, stripPrefix)
import Data.Maybe (fromJust)
import Data.String.Utils
import Data.Map.Strict (Map, empty, insert)

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

newGrammar :: Metagrammar Char -> Grammar Char
newGrammar meta = Grammar meta empty

data LSystem a = LSystem {
  options     :: Map [a] [a],
  macros      :: Map [a] [a],
  grammar     :: Grammar a,
  axiom       :: [a]
  } deriving (Show)

parseRuleFile :: String -> LSystem Char
parseRuleFile text =
  foldl' addLineToSystem emptySystem $ filterComments $ lines text

addOption :: Ord a => [a] -> [a] -> LSystem a -> LSystem a
addOption k v (LSystem o m g a) = LSystem (insert k v o) m g a

addMacro :: Ord a => [a] -> [a] -> LSystem a -> LSystem a
addMacro k v (LSystem o m g a) = LSystem o (insert k v m) g a

setAxiom :: Show a => [a] -> LSystem a -> LSystem a
setAxiom a (LSystem opt mac gram _) = LSystem opt mac gram a

setGrammar :: Grammar a -> LSystem a -> LSystem a
setGrammar a (LSystem opt mac _ ax) = LSystem opt mac a ax

emptySystem :: LSystem Char
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

addLineToSystem :: LSystem Char -> String -> LSystem Char
addLineToSystem sys [] = sys
addLineToSystem sys line
  | ':' `elem` line = addParam line sys
  | otherwise       = addRule line sys

addParam :: String -> LSystem Char -> LSystem Char
addParam line sys
  | "axiom:" `isPrefixOf` line =
    setAxiom (argFor "axiom:") sys
  | "define:" `isPrefixOf` line =
    let (name, def) = break isSpace $ cleanArgument "define:" line
    in
      addMacro name (strip def) sys
  | "delta:" `isPrefixOf` line =
    addOption "delta" (cleanArgument "delta:" line) sys
  | "ignore:" `isPrefixOf` line =
    setIgnore (cleanArgument "ignore:" line) sys
  | "iterate:" `isPrefixOf` line =
    addOption "iterate" (cleanArgument "iterate:" line) sys
  | "stepRatio:" `isPrefixOf` line =
    addOption "stepRatio" (cleanArgument "stepRatio:" line) sys
  | otherwise = sys
  where argFor = cleanArgument line

cleanArgument :: String -> String -> String
cleanArgument str argName = strip $ fromJust $ stripPrefix argName str

setIgnore :: String -> LSystem Char -> LSystem Char
setIgnore toIgnore sys =
  setGrammar (setMetagrammar (metagrammar toIgnore) $ grammar sys) sys

addRule :: String -> LSystem Char -> LSystem Char
addRule line sys =
  case parseRule (getMetagrammar $ grammar sys) line of
    Just specAndProd -> setGrammar (addRuleFromSpec specAndProd $ grammar sys) sys
    Nothing -> sys

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
