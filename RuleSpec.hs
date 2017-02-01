module RuleSpec (
    RuleSpec(..)
  , SpecTerm(..)
  , SpecFactor(..)
  , ContextMatch(..)
  , TermMatch(..)
  , FactorMatch(..)
  , wildness
  , termMatchLength
  , specTermLength
  , addContextBindings
  , matchContext
  , matchLongestTerm
  , Metagrammar
  , Tape
  ) where
import Error
import Metagrammar
import Tape
import Utils
import NumEval
import Control.Monad (join)
import Data.Char
import Data.List (isPrefixOf)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Number
import qualified Text.ParserCombinators.Parsec.Token as Token

data RuleSpec = RuleSpec {
    rsMeta  :: Metagrammar
  , rsLeft  :: SpecTerm
  , rsPred  :: SpecTerm
  , rsRight :: SpecTerm
  } deriving (Eq, Show)


wildness :: RuleSpec -> Int
wildness a =
  let wilds = map (SpecWild ==) [rsLeft a, rsRight a] in
    foldl (\acc w -> if w then acc + 1 else acc) 0 wilds
    
data SpecTerm = SpecWild | SpecTerm [SpecFactor] deriving (Eq, Show)

-- Need this for rule lookup on predecessor.
instance Ord SpecTerm where
  compare SpecWild SpecWild = EQ
  compare SpecWild _        = GT
  compare _ SpecWild        = LT
  compare (SpecTerm fa) (SpecTerm fb) = compare fa fb

specTermLength :: SpecTerm -> Int
specTermLength SpecWild = 0
specTermLength (SpecTerm factors) =
  sum $ map (\(SpecFactor name params) -> length name + length params) factors

data SpecFactor = SpecFactor {
    sfName   :: String
  , sfParams :: [String]
  } deriving (Eq, Ord, Show)

nullFactor :: SpecFactor
nullFactor = SpecFactor "" []

data ContextMatch = ContextMatch {
    cmLeft  :: TermMatch
  , cmPred  :: TermMatch
  , cmRight :: TermMatch
  }

data TermMatch = TermMatch {
    tmTerm    :: SpecTerm
  , tmFactors :: [FactorMatch]
  } deriving (Show)

data FactorMatch = FactorMatch {
    fmFactor :: SpecFactor
  , fmLength :: Int         -- Only for predecessor term
  , fmArgs   :: [Double]    -- Parsed numerical arguments
  , fmNext   :: Tape        -- Follows matched item
  }

instance Show FactorMatch where
  show f = "FactorMatch " ++ show (fmFactor f) ++ " " ++ show (fmArgs f)

termMatchLength :: TermMatch -> Int
termMatchLength (TermMatch _ factors) = sum $ map fmLength factors

addContextBindings :: ContextMatch -> Bindings -> Bindings
addContextBindings ctx bindings =
  foldl (\b (k, v) ->
           bindScalar k (Evaluator (\_ -> return v)) b) bindings $
  join $ join $
  map ((map (\fm -> zip (sfParams $ fmFactor fm ) (fmArgs fm)) . tmFactors)
       . (\f -> f ctx))
  [cmLeft, cmPred, cmRight]
  
--  fold join map(map(zip)) map 

matchContext :: TermMatch -> RuleSpec -> Tape -> ErrorM (Maybe ContextMatch)
matchContext pred spec@(RuleSpec meta l (SpecTerm p) r) t = do
  leftMatch <- lcondiff meta l t `amendE'` "matchSpec"
  case leftMatch of
    Nothing -> return Nothing
    Just lm -> do
      rightMatch <- rcondiff meta r t `amendE'` "matchSpec"
      case rightMatch of
        Nothing -> return Nothing
        Just rm -> return $ Just $ ContextMatch lm pred rm

-- Check left context
lcondiff :: Metagrammar -> SpecTerm -> Tape -> ErrorM (Maybe TermMatch)
lcondiff _ SpecWild _ = return $ Just $ TermMatch SpecWild []
lcondiff meta spec@(SpecTerm factors) t = do
  matched <- lcondiffRec meta factors t
  if length factors == length matched then
    return $ Just $ TermMatch spec matched
    else return Nothing

-- Check left context
lcondiffRec :: Metagrammar -> [SpecFactor] -> Tape -> ErrorM [FactorMatch]
lcondiffRec _ [] _ = return []
lcondiffRec meta s@(x:xs) t =
  let h = tapeAtHead t
      name = sfName x
      diffNext = moveLeft t >>= lcondiffRec meta xs
    in case () of
      _ | isAtStart t          -> return []
        | isSpace h            -> diffNext
        | isIgnored meta h     -> diffNext
        | isOpenPunctuation h  -> diffNext
        | h == ')' -> do
            (argStr, t') <- skipAndCopyLeft t
            args <- gatherArgs argStr
            if length args == length (sfParams x) then do
              t'' <- moveLeftBy (length name) t'
              if name `isPrefixOf` tapeHead t'' then
                let fm = FactorMatch x (distance t'' t) args t'' in do
                  rest <- lcondiffRec meta xs t''
                  return (fm : rest)
              else return []
            else return []
        | isClosePunctuation h -> skipLeft t >>= lcondiffRec meta s
        | otherwise -> do
            t' <- moveLeftBy (length name) t
            if name `isPrefixOf` tapeHead t' then do
              rest <- lcondiffRec meta xs t'
              let fm = FactorMatch x (distance t' t) [] t' in
                return (fm : rest)
            else return []

-- Check left context
rcondiff :: Metagrammar -> SpecTerm -> Tape -> ErrorM (Maybe TermMatch)
rcondiff _ SpecWild _ = return $ Just $ TermMatch SpecWild []
rcondiff meta spec@(SpecTerm factors) t = do
  t' <- moveRight t
  matched <- rcondiffRec meta factors t'
  if length factors == length matched then
    return $ Just $ TermMatch spec matched
    else return Nothing

-- Check right context
rcondiffRec :: Metagrammar -> [SpecFactor] -> Tape -> ErrorM [FactorMatch]
rcondiffRec _ [] _ = return []
rcondiffRec meta s@(x:xs) t =
  let h = tapeAtHead t
      name = sfName x
      diffNext = moveRight t >>= rcondiffRec meta xs
    in case () of
      _ | isAtEnd t           -> return []
        | isSpace h           -> diffNext
        | isIgnored meta h    -> diffNext
        | isOpenPunctuation h -> skipRight t >>= rcondiffRec meta s
        | name `isPrefixOf` tapeHead t -> do
          t' <- moveRightBy (length name) t
          if '(' == tapeAtHead t' then do
            (argStr, t'') <- skipAndCopy t'
            args <- gatherArgs argStr
            if length args == length (sfParams x) then
              let fm = FactorMatch x (distance t'' t) args t'' in do
                rest <- rcondiffRec meta xs t''
                return (fm : rest)
            else return []
          else 
            let fm = FactorMatch x (length name) [] t in do
              rest <- rcondiffRec meta xs t'
              return (fm : rest)
        | otherwise -> return []

matchLongestTerm :: Metagrammar -> [SpecTerm] -> Tape ->
  ErrorM (Maybe TermMatch)
matchLongestTerm _ [] _ = return Nothing
matchLongestTerm meta (s@(SpecTerm factors):px) t = do
  isPfx <- isPrefixOfIgnoring meta factors t
  if null isPfx then matchLongestTerm meta px t
    else return $ Just $ TermMatch s isPfx

isPrefixOfIgnoring :: Metagrammar -> [SpecFactor] -> Tape ->
  ErrorM [FactorMatch]
isPrefixOfIgnoring _ [] _ = return []
isPrefixOfIgnoring meta term@(f:fs) t
  | isAtEnd t = return []
  | isIgnored meta (tapeAtHead t) = do
      t' <- moveRight t `amendE'` "isPrefixOfIgnoring"
      isPrefixOfIgnoring meta term t'
  | otherwise = do
    match <- matchFactor meta f t
    case match of
      Just fm  -> do
        rest <- isPrefixOfIgnoring meta fs (fmNext fm)
        return (fm : rest)
      _ -> return []

matchFactor :: Metagrammar -> SpecFactor -> Tape -> ErrorM (Maybe FactorMatch)
matchFactor meta fact@(SpecFactor name params) t
  | name `isPrefixOf` tapeHead t = do
    afterName <- moveRightBy (length name) t
    if null params then
      return $ Just $ FactorMatch fact (length name) [] afterName
    else if tapeAtHead afterName /= '(' then return Nothing
    else do
      (argStr, t') <- skipAndCopy t `amendE'` "matchFactor"
      args <- gatherArgs argStr
      let d = distance t t' in
        if length args == length params then
          return $ Just $
            FactorMatch fact (distance t' t) args t'
        else return Nothing
  | otherwise = return Nothing

--

argLexer :: Token.GenTokenParser String st Identity
argLexer = Token.makeTokenParser emptyDef

gatherArgs :: String -> ErrorM [Double]
gatherArgs args =
  case parse argsParse "" args of
    Left err -> throwE' $ show err
    Right x  -> return x

commaSep1 = Token.commaSep1 argLexer
float     = Token.float argLexer
integer   = Token.integer argLexer

argsParse = commaSep1 RuleSpec.number

number = try float <|> liftM fromInteger integer



