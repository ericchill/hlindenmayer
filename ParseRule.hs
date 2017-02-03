module ParseRule (
    testParse
  , parseRule
  , Expr(..)
  , ParsedRule(..)
  , ParsedProduction(..)
  , ParsedProdFactor(..)
  , TokenParser
  , lexer
  ) where
import Error
import Rule
import RuleSpec
import NumEval
import NumEval.Parser
import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import Data.String.Utils (strip)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Number
import qualified Text.ParserCombinators.Parsec.Token as Token

type ParsedRule = (RuleSpec, ParsedProduction)

data ParsedProduction = ParsedProduction {
    ppCond    :: Maybe NumExpr
  , ppFactors :: [ParsedProdFactor]
  , ppProb    :: Maybe NumExpr
  } deriving (Show)
  
data ParsedProdFactor = ParsedProdFactor {
     ppfName     :: String
  ,  ppfArgExprs :: [ParseRule.Expr]
  } deriving (Show)

data Expr = Numeric NumExpr | Stringy String deriving (Show)

languageDef :: LanguageDef st
languageDef = emptyDef

type TokenParser st = Token.GenTokenParser String st Identity

lexer :: TokenParser st
lexer = Token.makeTokenParser languageDef

commaSep1     = Token.commaSep1     lexer
float         = Token.float         lexer
identifier    = Token.identifier    lexer
integer       = Token.integer       lexer
parens        = Token.parens        lexer
symbol        = Token.symbol        lexer
stringLiteral = Token.stringLiteral lexer
whitespace    = Token.whiteSpace    lexer


testParse :: Metagrammar -> String -> String -> ErrorM ParsedRule
testParse meta filename line =
  case parse (rule meta) filename line of
    Left err -> throwE' $ show err
    Right prod -> return prod

parseRule :: Metagrammar -> String -> String ->
  ErrorM (RuleSpec, ParsedProduction)
parseRule meta filename line =
  case parse (rule meta) filename line of
    Left err -> throwE' $ show err
    Right prod -> return prod

rule meta = do
  whitespace
  spec <- ruleSpec meta <?> "ruleSpec"
  cond <- ruleCondition-- <?> "ruleCondition"
  prod <- prodTerm cond <?> "prodTerm"
  eof
  return (spec, prod)

ruleSpec meta = do
  (left, pred, right) <- rule2Spec <|> rule1LSpec <|> rule1RSpec <|> rule0Spec
  return $ RuleSpec meta left pred right

ruleCondition = condExpr <|> nullCond

condExpr = try $ do
  cond <- between (symbol ";") (symbol "->") NumEval.Parser.expr
  return $ Just cond

nullCond = do
  symbol "->"
  return Nothing
  
rule0Spec = do
  term <- specTerm
  return (SpecWild, term, SpecWild)

rule1LSpec = try $ do
  left <- specTermOrWild
  symbol "<"
  pred <- specTerm
  return (left, pred, SpecWild)

rule1RSpec = try $ do
  pred <- specTerm
  symbol ">"
  right <- specTermOrWild
  return (SpecWild, pred, right)

rule2Spec = try $ do
  (left, pred, _) <- rule1LSpec
  symbol ">"
  right <- specTermOrWild
  return (left, pred, right)

specTermOrWild = wild <|> specTerm

wild = try $ do
  symbol "*"
  return SpecWild

specTerm = do
  factors <- many specFactor
  let f' = map (\f -> f { sfName = strip (sfName f) }) factors in
    return $ SpecTerm $ filter (not . null . sfName) f'

specFactor = do
  name <- turtleOp
  params <- option [] $ try $ parens (commaSep1 identifier)
  return $ SpecFactor name params

prodTerm cond = do
  factors <- many prodFactor
  prob <- optionMaybe prodProbability
  let f' = map (\f -> f { ppfName = strip (ppfName f) }) factors
      f'' = filter (not . null . ppfName) f'
    in
    return $ ParsedProduction cond f'' prob

prodProbability = try $ do
  symbol ":"
  NumEval.Parser.expr

prodFactor = do
  name <- turtleOp
  exprs <- option [] $ parens $ commaSep1 ParseRule.expr
  return $ ParsedProdFactor name exprs

expr =
  try (do {
    e <- NumEval.Parser.expr;
    return $ Numeric e
    })
  <|> try (do {
          e <- stringLiteral;
          return $ Stringy e
          })

turtleOp = try (extendedOp <|> loneMinus <|> almostAnythingElse)

extendedOp = try $ do
  symbol "@"
  identifier

loneMinus = try $ do
  symbol "-"
  notFollowedBy $ symbol ">"
  return "-"

almostAnythingElse = try $ do
  c <- noneOf "-:<>;"
  return [c]
