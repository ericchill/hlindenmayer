module Parse (
  parseRuleFile,
  module Grammar,
  module LSystem
  )
where
import LSystem
import Metagrammar
import Grammar
import RuleSpec
import Turtle (TAction, encodeActions, Turt)
import Utils
import Prelude hiding (lookup, null)
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf, null, stripPrefix)
import Data.Maybe (fromJust, fromMaybe)
import Data.String.Utils
import qualified Data.Map.Strict as Map

{-
  rule
    | [stuff <] word [> stuff] --> words
  stuff
    | *
    | words
-}

metagrammar :: String -> Metagrammar Char
metagrammar toIgnore = Metagrammar {
  isOpenBracket  = (`elem` "(["),
  isCloseBracket = (`elem` ")]"),
  closesBracket  = \a b ->
      case a of
        '(' -> b == ')'
        '[' -> b == ']'
        _   -> False,
  isBlank        = isSpace,
  isIgnored      = (`elem` toIgnore),
  isWild         = (== '*'),
  isSkipBalanced = (== '%'),
  rsSig          = "()[]*"
  }

parseRuleFile :: (Turt a) => String -> LSystemError a Char
parseRuleFile text =
  foldM addLineToSystem
        (emptySystem $ metagrammar "")
        (filterComments $ lines text)

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

addLineToSystem :: (Turt a) => LSystem a Char -> String -> LSystemError a Char
addLineToSystem sys [] = return sys
addLineToSystem sys line
  | "-->" `isInfixOf` line = addRule line sys
  | ':' `elem` line        = addParam line sys
  | otherwise = throwE $ "Don't know what to do with: " ++ line

addParam :: (Turt a) => String -> LSystem a Char -> LSystemError a Char
addParam line sys
  | name == "axiom"  = return $ setAxiom def sys
  | name == "define" = addMacro name def sys
  | null name        = throwE $ "Malformed option: " ++ line
  | otherwise        = return $ addOption name def sys
  where name = strip $ takeWhile (':' /=) line
        def = strip $ tail $ dropWhile (':' /=) line

addRule :: (Turt a) => String -> LSystem a Char -> LSystemError a Char
addRule line sys =
  do
    specAndProd <- parseRule (getMetagrammar $ lGrammar sys)
      (trace' ("addRule " ++ line) line)
    return $ setGrammar (addRuleFromSpec specAndProd $ lGrammar sys) sys

parseRule :: Metagrammar Char -> String -> ErrorM (RuleSpec Char, String)
parseRule meta input
  | length sides == 2 =
    if null specStr then throwE $ "parseRule input is " ++ input
    else do
      spec <- parseRuleSpec meta specStr
      return (spec, strip prodStr)
  | otherwise = throwE $ "parseRule2 input is " ++ input
  where sides = split "-->" input
        specStr = head sides
        prodStr = sides !! 1

parseRuleSpec :: Metagrammar Char -> String -> ErrorM (RuleSpec Char)
parseRuleSpec meta [] = throwE "Empty RuleSpec string."
parseRuleSpec meta str
  | 2 == length pieces =
      parseRuleSpec2 meta ((reverse . head) pieces) $ pieces !! 1
  | otherwise =
      parseRuleSpec2 meta "*" $ head pieces
  where pieces = map strip $ split "<" str

parseRuleSpec2 :: Metagrammar Char -> String -> String -> ErrorM (RuleSpec Char)
parseRuleSpec2 meta _ [] = error "No RuleSpec after left constraint"
parseRuleSpec2 meta left str
  | 2 == length pieces = case head pieces of
      "" -> throwE "No center element in RuleSpec."
      center -> case tail pieces of
                  [""]  -> throwE "Empty right constraint in RuleSpec."
                  right -> return $ RuleSpec meta left center $ head right
  -- TODO add case for left side only that specifies deletion
  | otherwise = return $ RuleSpec meta left str "*"
  where pieces = map strip $ split ">" str
