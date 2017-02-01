module Parse (
  parseRuleFile,
  metagrammar,
  module Grammar,
  module LSystem
  )
where
import Grammar
import LSystem
import Metagrammar
import NumEval
import NumEval.Translate
import ParseRule
import Rule
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
  | ':' `elem` line       = addParam line sys
  | otherwise             = throwE' $ "Don't know what to do with: " ++ line

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
    (spec, prod) <- parseRule (gMeta $ lGrammar sys) line line
    prod' <- translateProduction prod
    return $ setGrammar sys (addRuleFromSpec spec prod' $ lGrammar sys)

translateProduction :: ParsedProduction -> ErrorM Production
translateProduction parsed = do
  factors <- mapM translateFactor (ppFactors parsed)
  cond <- translateMaybeNumExpr (ppCond parsed) 1.0
  prob <- translateMaybeNumExpr (ppProb parsed) 1.0
  return $ Production (Just cond) factors (Just prob)

translateFactor :: ParsedProdFactor -> ErrorM ProdFactor
translateFactor parsed = do
  exprs <- mapLeft $ mapM translate (ppfArgExprs parsed)
  return $ ProdFactor (ppfName parsed) exprs

translateMaybeNumExpr :: Maybe NumExpr -> Double -> ErrorM Evaluator
translateMaybeNumExpr expr def =
  case expr of
    Just e  -> mapLeft $ translate e
    Nothing -> return $ Evaluator (\_ -> return def)
    
