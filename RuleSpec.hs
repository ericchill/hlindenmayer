module RuleSpec (
    RuleSpec(..)
  , SpecTerm(..)
  , SpecFactor(..)
  , SpecMatch(..)
  , TermMatch(..)
  , FactorMatch(..)
  , specTermLength
  , matchSpec
  , matchLongestTerm
  , Metagrammar
  , Tape
  ) where
import Metagrammar
import Tape
import Utils
import NumEval
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

data SpecMatch = SpecMatch {
    tmLeft  :: TermMatch
  , tmPred  :: TermMatch
  , tmRight :: TermMatch
  }

data TermMatch = TermMatch {
    tmTerm    :: SpecTerm
  , tmFactors :: [FactorMatch]
  }

data FactorMatch = FactorMatch {
    fmFactor :: SpecFactor
  , fmLength :: Int         -- Only for predecessor term
  , fmArgs   :: [Double]    -- Parsed numerical arguments
  , fmNext   :: Tape        -- Beginning of matched item
  }

endMatch :: Tape -> FactorMatch
endMatch = FactorMatch nullFactor 0 []

matchSpec :: RuleSpec -> Tape -> ErrorM Bool
matchSpec spec@(RuleSpec meta l (SpecTerm p) r) t = do
  prefixMatches <- isPrefixOfIgnoring meta p t `amendE'` "prefixMatches"
  case prefixMatches of
    Just n -> do
      leftMatch <- lcondiff meta l t `amendE'` "matchSpec"
      case leftMatch of
        Nothing -> return False
        Just lm -> do
          rightMatch <- rcondiff meta l t `amendE'` "matchSpec"
          case rightMatch of
            Nothing -> return False
            Just rm -> return True
{-
      (lcondiff meta l t &&&& rcondiff meta r t)
                      `amendE'` ("matchSpec " ++ show l ++ " < " ++
                                 show p ++
                                 " > " ++ show r ++
                                 ", t = " ++ tShow t)
-}
    _ -> return False

-- Check left context
lcondiff :: Metagrammar -> SpecTerm -> Tape -> ErrorM (Maybe TermMatch)
lcondiff _ SpecWild t = return $ Just $ TermMatch SpecWild []
lcondiff meta spec@(SpecTerm factors) t = do
  diff <- lcondiffRec meta factors t
  case diff of
    Just terms -> return $ Just $ TermMatch spec terms
    Nothing    -> return Nothing

-- Check left context
lcondiffRec :: Metagrammar -> [SpecFactor] -> Tape ->
  ErrorM (Maybe [FactorMatch])
lcondiffRec _ [] _ = return $ Just []
lcondiffRec meta s@(x:xs) t =
  let h = tapeAtHead t
      name = sfName x
      diffNext = moveLeft t >>= lcondiffRec meta xs
    in case () of
      _ | isAtStart t           -> return Nothing
        | isSpace h             -> diffNext
        | isIgnored meta h      -> diffNext
        | isOpenBracket meta h  -> diffNext
        | isCloseBracket meta h -> skipLeft meta t >>= lcondiffRec meta s
        | h == ')' -> do
            (argStr, t') <- skipAndCopyLeft meta t
            args <- gatherArgs argStr
            if length args == length (sfParams x) then do
              t'' <- moveLeftBy (length name) t'
              if name `isPrefixOf` tapeHead t'' then do
                rest <- lcondiffRec meta xs t''
                case rest of
                  Just fms ->
                    let fm = FactorMatch x (distance t'' t) args t'' in
                      return $ Just (fm : fms)
                else return Nothing
            else return Nothing
        | otherwise -> do
          t' <- moveLeftBy (length name) t
          if name `isPrefixOf` tapeHead t' then do
            rest <- lcondiffRec meta xs t'
            case rest of
              Just fms ->
                let fm = FactorMatch x (distance t' t) [] t' in
                  return $ Just (fm : fms)
          else return Nothing

-- Check left context
rcondiff :: Metagrammar -> SpecTerm -> Tape -> ErrorM (Maybe TermMatch)
rcondiff _ SpecWild _ = return $ Just $ TermMatch SpecWild []
rcondiff meta spec@(SpecTerm factors) t = do
  diff <- rcondiffRec meta factors t
  case diff of
    Just terms -> return $ Just $ TermMatch spec terms
    Nothing    -> return Nothing

-- Check right context
rcondiffRec :: Metagrammar -> [SpecFactor] -> Tape ->
  ErrorM (Maybe [FactorMatch])
rcondiffRec _ [] _ = return $ Just []
rcondiffRec meta s@(x:xs) t =
  let h = tapeAtHead t
      name = sfName x
      diffNext = moveRight t >>= rcondiffRec meta xs
    in case () of
      _ | isAtEnd t            -> return Nothing
        | isSpace h            -> diffNext
        | isIgnored meta h     -> diffNext
        | isOpenBracket meta h -> skipRight meta t >>= rcondiffRec meta s
        | name `isPrefixOf` tapeHead t -> do
          t' <- moveRightBy (length name) t
          if '(' == tapeAtHead t' then do
            (argStr, t'') <- skipAndCopy meta t'
            args <- gatherArgs argStr
            if length args == length (sfParams x) then do
              rest <- rcondiffRec meta xs t''
              case rest of
                Just fms ->
                  let fm = FactorMatch x (distance t'' t) args t'' in
                    return $ Just (fm : fms)
            else return Nothing
          else do
            rest <- rcondiffRec meta xs t'
            case rest of
              Just fms ->
                let fm = FactorMatch x (length name) [] t in
                  return $ Just (fm : fms)
-- [maybe, nothing, 
matchLongestTerm :: Metagrammar -> [SpecTerm] -> Tape ->
  ErrorM (Maybe TermMatch)
matchLongestTerm _ [] _ = return Nothing
matchLongestTerm meta (s@(SpecTerm factors):px) t = do
  isPfx <- isPrefixOfIgnoring meta factors t
  case isPfx of
    Just match -> return $ Just $ TermMatch s match
    Nothing    -> matchLongestTerm meta px t

isPrefixOfIgnoring :: Metagrammar -> [SpecFactor] -> Tape ->
  ErrorM (Maybe [FactorMatch])
isPrefixOfIgnoring _ [] _ = return $ Just []
isPrefixOfIgnoring meta term@(f:fs) t
  | isAtEnd t = return Nothing
  | isIgnored meta (tapeAtHead t) = do
      t' <- moveRight t `amendE'` "isPrefixOfIgnoring"
      matchRest <- isPrefixOfIgnoring meta term t'
      case matchRest of
        Just fms -> return $ Just $ FactorMatch nullFactor 1 [] t : fms
        Nothing  -> return Nothing
  | otherwise = do
    match <- matchFactor meta f t
    case match of
      Just fm  -> do
        rest <- isPrefixOfIgnoring meta fs (fmNext fm)
        case rest of
          Just fms ->
            return $ Just (fm : fms)
          _ -> return Nothing
      _ -> return Nothing

matchFactor :: Metagrammar -> SpecFactor -> Tape -> ErrorM (Maybe FactorMatch)
matchFactor meta fact@(SpecFactor name params) t
  | name `isPrefixOf` tapeHead t = do
    afterName <- moveRightBy (length name) t
    if null params then
      return $ Just $ FactorMatch fact (length name) [] afterName
    else if tapeAtHead afterName /= '(' then return Nothing
    else do
      (argStr, t') <- skipAndCopy meta t `amendE'` "matchFactor"
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



