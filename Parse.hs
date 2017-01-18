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
import Data.List.Utils
import Data.Maybe (fromJust, fromMaybe)
import Data.String.Utils
import qualified Data.Map.Strict as Map

{-
  rule
    | [stuff <] word [> stuff] -> words
  stuff
    | * (blank is OK)
    | words
-}

metagrammar :: String -> Metagrammar
metagrammar toIgnore = Metagrammar {
  isOpenBracket  = isOpenPunctuation,
  isCloseBracket = isClosePunctuation,
  closesBracket  = closes,
  isFuncArg      = (== '('),
  isBlank        = isSpace,
  isIgnored      = (`elem` toIgnore),
  isWild         = (== '*'),
  isBreak        = (== '%'),
  rsSig          = "()[]{}*%"
  }

parseRuleFile :: (Turt a) => String -> LSystemError a
parseRuleFile text =
  foldM addLineToSystem
        (emptySystem $ metagrammar "")
        (filterComments $ lines text)

addLineToSystem :: (Turt a) => LSystem a -> String -> LSystemError a
addLineToSystem sys [] = return sys
addLineToSystem sys line
  | "->" `isInfixOf` line = addRule line sys
  | ':' `elem` line        = addParam line sys
  | otherwise              = throwE' $ "Don't know what to do with: " ++ line

filterComments :: [String] -> [String]
filterComments ls =
  filter (not . null) $
  filter (not . startswith "//") $
  fst $ foldr (\l (res, incomment) ->
                if incomment then
                  if "/*" `isPrefixOf` l then (res, False) else (res, True)
                else
                  if "*/" `isPrefixOf` l then (res, True) else (l : res, False)

              ) ([], False) ls

addParam :: (Turt a) => String -> LSystem a -> LSystemError a
addParam line sys
  | param == "axiom"     = return $ setAxiom sys arg
  | param == "define"    = addMacro sys `uncurry` stripSplit1 " " arg
  | param == "ignore"    = return $ setIgnore sys arg
  | param == "delta"     = addFloatOption sys param arg
  | param == "pen_scale" = addFloatOption sys param arg
  | null param        = throwE' $ "Malformed option: " ++ line
  | otherwise         = return $ addOption sys param arg
  where (param, arg) = stripSplit1 ":" line


addRule :: (Turt a) => String -> LSystem a -> LSystemError a
addRule line sys =
  do
    (spec, production) <- parseRule (gMeta $ lGrammar sys) line
    return $ setGrammar sys (addRuleFromSpec spec production $ lGrammar sys)

parseRule :: Metagrammar -> String -> ErrorM (RuleSpec, String)
parseRule meta input =
  if null specStr then throwE' $ "parseRule input is " ++ input
  else do
    spec <- parseRuleSpec meta specStr
    return (spec, strip prodStr)
  where (specStr, prodStr) = stripSplit1 "-->" input

parseRuleSpec :: Metagrammar -> String -> ErrorM RuleSpec
parseRuleSpec meta [] = throwE' "Empty RuleSpec string."
parseRuleSpec meta str
  | not $ null right = parseRuleSpec2 meta left right
  | otherwise     = parseRuleSpec2 meta "*" left
  where
    (left, right) = stripSplit1 "<" str

parseRuleSpec2 :: Metagrammar -> String -> String -> ErrorM RuleSpec
parseRuleSpec2 meta _ [] = error "No RuleSpec after left constraint"
parseRuleSpec2 meta left str
  | not $ null right =
    case center of
      "" -> throwE' "No center element in RuleSpec."
      center -> return $ RuleSpec meta left center right
  | otherwise = return $ RuleSpec meta left str "*"
  where
    (center, right) = stripSplit1 ">" str

