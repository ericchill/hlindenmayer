module ParseRule (
    testParse
  , parseRule
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
import NumEval.Syntax
import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
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
  ,  ppfArgExprs :: [NumExpr]
  } deriving (Show)
  
languageDef :: LanguageDef st
languageDef =
  emptyDef { Token.reservedOpNames = [
               "+", "-", "*", "/", "%", "^", "!",
                 "<", "<=", "==", ">=", ">", "!=",
                 "?", ":",
                 "&&", "||", "~",
                 "@"]
           }


type TokenParser st = Token.GenTokenParser String st Identity

lexer :: TokenParser st
lexer = Token.makeTokenParser languageDef

braces      = Token.braces     lexer
colon       = Token.colon      lexer
comma       = Token.comma      lexer
commaSep1   = Token.commaSep1  lexer
float       = Token.float      lexer
identifier  = Token.identifier lexer
integer     = Token.integer    lexer
lexeme      = Token.lexeme     lexer
parens      = Token.parens     lexer
reserved    = Token.reserved   lexer
reservedOp  = Token.reservedOp lexer
symbol      = Token.symbol     lexer
whitespace  = Token.whiteSpace lexer


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
  cond <- between (symbol ",") (symbol "->") (NumEval.Parser.expr <?> "expr")
  return $ Just cond

nullCond = do
  symbol "->"
  return Nothing
  
rule0Spec = try $ do
  term <- specTerm
  return (constWild, term, constWild)

rule1LSpec = try $ do
  left <- specTermOrWild <?> "r1ls"
  reservedOp "<"
  pred <- specTerm  <?> "r1ls (2)"
  return (left, pred, constWild)

rule1RSpec = try $ do
  pred <- specTerm <?> "r1rs"
  reservedOp ">"
  right <- specTermOrWild  <?> "r1rs (2)"
  return (constWild, pred, right)

rule2Spec = try $ do
  (left, pred, _) <- rule1LSpec
  reservedOp ">"
  right <- specTermOrWild  <?> "r2ls"
  return (left, pred, right)

constWild :: SpecTerm
constWild = SpecWild

specTermOrWild = wild <|> specTerm

wild = try $ do
  reservedOp "*"
  return constWild

specTerm = do
  factors <- many specFactor
  return $ SpecTerm factors

specFactor = do
  name <- turtleOp <?> "turtleOp"
  params <- option [] $ try (parens (commaSep1 identifier) <?> "why?")
  return $ SpecFactor name params

prodTerm cond = do
  factors <- many (prodFactor <?> "prodFactor")
  prob <- optionMaybe prodProbability
  return $ ParsedProduction cond factors prob

prodProbability = try $ do
  reservedOp ":"
  NumEval.Parser.expr

prodFactor = do
  name <- turtleOp
  exprs <- option [] $ parens $ commaSep1 NumEval.Parser.expr
  return $ ParsedProdFactor name exprs

turtleOp = try (
  extendedOp <|> loneMinus <|> almostAnythingElse)

extendedOp = try $ do
  reservedOp "@"
  identifier

loneMinus = try $ do
  reservedOp "-"
  notFollowedBy $ reservedOp ">"
  return "-"

almostAnythingElse = try $ do
  c <- noneOf "-:<>,"
  return [c]
