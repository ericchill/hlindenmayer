module Parse (
  LSystem(..),
  parseRuleFile,
  Grammar
  )
where
import Metagrammar
import Grammar
import RuleSpec
import Prelude hiding (null)
--import Control.Monad
import Data.Char (isSpace)
import Data.List (foldl', isPrefixOf, stripPrefix)
import Data.Maybe (fromJust)
import Data.String.Utils
import Data.Map.Strict (Map, empty, insert)

{-
  rule
    | expr < token > expr --> production
    | expr < token        --> production
    | token > expr        --> production
    | token               --> production
  expr
    | *
    | [token]
-}

metagrammar :: String -> Metagrammar Char
metagrammar toIgnore = Metagrammar
  (\a -> a `elem` "([")
  (\a -> a `elem` ")]")
  (\a b -> case a of
      '(' -> b == ')'
      '[' -> b == ']'
      _   -> False
  )
  (\a -> isSpace a)
  (\a -> a `elem` toIgnore)
  (\a -> a == '*')
  (\a -> a == '%')
  "()[]*"

newGrammar :: Metagrammar Char -> Grammar Char
newGrammar meta = Grammar meta empty

data LSystem a = LSystem {
  options     :: Map [a] [a],
  macros      :: Map [a] [a],
  grammar     :: Grammar a,
  axiom       :: [a]
  }

addOption :: Ord a => [a] -> [a] -> LSystem a -> LSystem a
addOption k v (LSystem o m g a) = LSystem (insert k v o) m g a

addMacro :: Ord a => [a] -> [a] -> LSystem a -> LSystem a
addMacro k v (LSystem o m g a) = LSystem o (insert k v m) g a

setAxiom :: [a] -> LSystem a -> LSystem a
setAxiom a (LSystem opt mac gram _) = LSystem opt mac gram a

setGrammar :: Grammar a -> LSystem a -> LSystem a
setGrammar a (LSystem opt mac _ ax) = LSystem opt mac a ax

emptySystem :: LSystem Char
emptySystem = LSystem empty empty (newGrammar $ metagrammar "") ""

parseRuleFile :: [Char] -> LSystem Char
parseRuleFile text =
  foldl' (\sys line -> addLineToSystem line sys) emptySystem $ filterComments $ lines text

filterComments :: [[Char]] -> [[Char]]
filterComments ls =
  fst $ foldr (\l (res, incomment) ->
                 if incomment then
                   if "{-" `isPrefixOf` l then (res, False) else (res, True)
                 else
                   if "-}" `isPrefixOf` l then (res, True) else (l : res, False)
              ) ([], False) $
  filter (\l@(x:_) -> (x /= '.') && (not $ "--" `isPrefixOf` l)) $ map strip ls

addLineToSystem :: [Char] -> LSystem Char -> LSystem Char
addLineToSystem [] sys = sys
addLineToSystem line sys
  | ':' `elem` line = addParam line sys
  | otherwise       = addRule line sys

addParam :: [Char] -> LSystem Char -> LSystem Char
addParam line sys
  | "axoim:" `isPrefixOf` line =
    setAxiom (cleanArgument "axoim:" line) sys
  | "define:" `isPrefixOf` line =
    let arg = cleanArgument "define:" line
        (name, def) = span (\c -> not $ isSpace c) arg
        cleanDef = strip def
    in
      addMacro name cleanDef sys
  | "delta:" `isPrefixOf` line =
    addOption "delta" (cleanArgument "delta:" line) sys
  | "ignore:" `isPrefixOf` line =
    setIgnore (cleanArgument "ignore:" line) sys
  | "iterate:" `isPrefixOf` line =
    addOption "iterate" (cleanArgument "iterate:" line) sys
  | "stepRatio:" `isPrefixOf` line =
    addOption "stepRatio" (cleanArgument "stepRatio:" line) sys
  | otherwise = sys

cleanArgument :: [Char] -> [Char] -> [Char]
cleanArgument argName str = strip $ fromJust $ stripPrefix argName str

setIgnore :: [Char] -> LSystem Char -> LSystem Char
setIgnore toIgnore sys =
  setGrammar (setMetagrammar (metagrammar toIgnore) $ grammar sys) sys

addRule :: [Char] -> LSystem Char -> LSystem Char
addRule line sys =
  case parseRule (getMetagrammar $ grammar sys) line of
    Just specAndProd -> setGrammar (addRuleFromSpec specAndProd $ grammar sys) sys
    Nothing -> sys

parseRule :: Metagrammar Char -> [Char] -> Maybe (RuleSpec Char, [Char])
parseRule meta input
  | length sides == 2 =
    if length specStr == 0 then Nothing
    else Just (parseRuleSpec meta specStr, strip prodStr)
  | otherwise = Nothing
  where sides = split "-->" input
        specStr = sides !! 0
        prodStr = sides !! 1

parseRuleSpec :: Metagrammar Char -> [Char] -> RuleSpec Char
parseRuleSpec meta [] = error "Empty RuleSpec string."
parseRuleSpec meta str
  | 2 == length pieces = parseRuleSpec2 meta (head pieces) $ pieces !! 1
  | otherwise          = parseRuleSpec2 meta "*" $ head pieces
  where pieces = split "<" str

parseRuleSpec2 :: Metagrammar Char -> [Char] -> [Char] -> RuleSpec Char
parseRuleSpec2 meta _ [] = error "No RuleSpec after left constraint"
parseRuleSpec2 meta left str
  | 2 == length pieces = case head pieces of
      "" -> error "No center element in RuleSpec."
      center  -> case tail pieces of
        [""]    -> error "Empty right constraint in RuleSpec."
        right   -> RuleSpec meta left center $ head right
  | otherwise = RuleSpec meta left str "*"
  where pieces = split ">" str
