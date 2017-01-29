{-# LANGUAGE FlexibleContexts #-}
module Turtle (
  TAction(..),
  Turt(..),
  encodeActions,
  foldActions,
  TPosition,
  TOrientation
  )
where
import Error
import Math
import Options
import Utils
import Control.Monad
import Data.List
import Data.Strict.Tuple
import Data.String.Utils
import Linear.V3
import System.IO

type TPosition = V3F
type TOrientation = M33F

---
--- The Turtle class
---
type TurtleMonad a = ErrorIO a

class Turt a where
  drawLine   :: a -> FloatArg a -> TurtleMonad a
  drawNoMark :: a -> FloatArg a -> TurtleMonad a
  move       :: a -> FloatArg a -> TurtleMonad a

  getPos           :: a -> TPosition
  setPos           :: a -> TPosition -> TurtleMonad a
  getOrientation   :: a -> TOrientation
  setOrientation   :: a -> TOrientation -> TurtleMonad a
  resetOrientation :: a -> TurtleMonad a

  drawSphere       :: a -> TurtleMonad a

  startPolygon     :: a -> TurtleMonad a
  markVertex       :: a -> TurtleMonad a
  endPolygon       :: a -> TurtleMonad a

  getPenWidth      :: a -> Double
  setPenWidth      :: a -> FloatArg a -> TurtleMonad a

  turnLeft :: a -> FloatArg a -> TurtleMonad a
  turnLeft tur = reorient tur zAxis

  turnRight :: a -> FloatArg a -> TurtleMonad a
  turnRight tur = reorientMinus tur zAxis

  pitchDown :: a -> FloatArg a -> TurtleMonad a
  pitchDown tur = reorientMinus tur yAxis

  pitchUp :: a -> FloatArg a -> TurtleMonad a
  pitchUp tur = reorient tur yAxis

  rollLeft :: a -> FloatArg a -> TurtleMonad a
  rollLeft tur = reorient tur xAxis

  rollRight :: a -> FloatArg a -> TurtleMonad a
  rollRight tur = reorientMinus tur xAxis

  turnAround :: a -> TurtleMonad a
  turnAround tur = reorient tur zAxis $ floatConst pi

  getAngle     :: a -> ErrorM Double
  setAngle     :: a -> FloatArg a -> TurtleMonad a
  
  shrinkPen :: a -> FloatArg a -> TurtleMonad a
  shrinkPen tur arg = do
    amt <- mapErrorM $ getFloatArg arg tur
    setPenWidth tur $ floatConst $ getPenWidth tur / amt
  
  growPen :: a -> FloatArg a -> TurtleMonad a
  growPen tur arg = do
    amt <- mapErrorM $ getFloatArg arg tur
    setPenWidth tur $ floatConst $ getPenWidth tur * amt

  invokeMacro :: a -> StringArg a -> TurtleMonad a
  invokeMacro tur arg = do
    macro <- mapErrorM $ getMacro tur arg
    foldActions macro tur

  getPenScale :: a -> ErrorM Double
  setPenScale :: a -> FloatArg a -> TurtleMonad a

  getTexture :: a -> ErrorM String
  setTexture :: a -> StringArg a -> TurtleMonad a

  getColor   :: a -> ErrorM String
  setColor   :: a -> StringArg a -> TurtleMonad a

  doAction :: a -> TAction a -> TurtleMonad a
  doAction t (Branch actions)   = foldActions actions t >> return t
  doAction t (DrawLine dt)      = drawLine t dt
  doAction t (DrawNoMark dt)    = drawLine t dt
  doAction t (Move dt)          = move t dt
  doAction t DrawSphere         = drawSphere t
  doAction t StartPolygon       = startPolygon t
  doAction t MarkVertex         = markVertex t
  doAction t EndPolygon         = endPolygon t
  doAction t (TurnLeft da)      = turnLeft t da
  doAction t (TurnRight da)     = turnRight t da
  doAction t (PitchDown da)     = pitchDown t da
  doAction t (PitchUp da)       = pitchUp t da
  doAction t (RollLeft da)      = rollLeft t da
  doAction t (RollRight da)     = rollRight t da
  doAction t TurnAround         = turnAround t
  doAction t ResetOrientation   = resetOrientation t
  doAction t (ShrinkPen a)      = shrinkPen t a
  doAction t (GrowPen a)        = growPen t a
  doAction t (SetPenWidth a)    = setPenWidth t a
  doAction t (SetPenScale a)    = setPenScale t a
  doAction t (InvokeMacro a)    = invokeMacro t a
  doAction t (SetTexture a)     = setTexture t a
  doAction t (SetColor a)       = setColor t a
  doAction t _ = return t

  getMacro     :: a -> StringArg a -> ErrorM [TAction a]
  getStringOpt :: a -> String -> String -> ErrorM String
  getFloatOpt  :: a -> String -> Double -> ErrorM Double

  reorient :: a -> V3F -> FloatArg a -> TurtleMonad a
  reorient tur axis arg = do
    angle <- mapErrorM $ getFloatArg arg tur
    setOrientation tur $ rotateMatrix (getOrientation tur) axis angle

  reorientMinus :: a -> V3F -> FloatArg a -> TurtleMonad a
  reorientMinus tur axis arg = do
    angle <- mapErrorM $ getFloatArg arg tur
    setOrientation tur $ rotateMatrix (getOrientation tur) axis (- angle)
    
  
foldActions :: (Turt a) => [TAction a] -> a -> TurtleMonad a
foldActions actions t =
  -- Strictness here saves about 50% memory in render phase.
  t `seq` foldM doAction t actions


data TAction a =
  Branch [TAction a] |
  DrawLine (FloatArg a) |
  DrawNoMark (FloatArg a) |
  Move (FloatArg a) |
  TurnLeft (FloatArg a) |
  TurnRight (FloatArg a) |
  PitchDown (FloatArg a) |
  PitchUp (FloatArg a) |
  RollLeft (FloatArg a) |
  RollRight (FloatArg a) |
  TurnAround |
  ResetOrientation |
  ShrinkPen (FloatArg a) |
  GrowPen (FloatArg a) |
  SetPenWidth (FloatArg a) |
  SetPenScale (FloatArg a) |
  SetColor (StringArg a) | -- Should be Vector arg
  SetTexture (StringArg a) |
  PlaceObject (StringArg a) |
  DrawSphere |
  StartPolygon |
  MarkVertex |
  EndPolygon |
  InvokeMacro (StringArg a) |
  SetValue (StringArg a) (FloatArg a) | -- plus stringy version ?
  PushTurtle |
  PopTurtle |
  Noop deriving (Show)

isNoop :: (Turt a) => TAction a -> Bool
isNoop Noop = True
isNoop _    = False

encodeActions :: (Turt a) => String -> ErrorM [TAction a]
encodeActions s = do
  (_, actions) <- encodeActionsRec s
  return actions

encodeActionsRec :: (Turt a) => String -> ErrorM (String, [TAction a])
encodeActionsRec [] = return ("", [])
encodeActionsRec s = do
  (rest, action) <- encodeAction s `amendE'` "encodeActionsRec"
  (rest', actions) <- encodeActionsRec rest
  if isNoop action then
    return (rest', actions)
  else
    return (rest', action : actions)

-- On error string is at error, otherwise it's after parsed action.
-- Likely cause of future errors will be in parsing expressions as arguments.
encodeAction :: (Turt a) => String -> ErrorM (String, TAction a)
encodeAction [] = return ("", Noop)
encodeAction s@(x:xs) =
  case () of
     _ | x == '['  -> encodeBranch xs
       -- Things with length dimension
       | x `elem` "FGfp" -> do
           (arg, xs') <- encodeArg xs $ floatConst 1.0
           case x of
             'F'  -> return (xs', DrawLine arg)
             'G'  -> return (xs', DrawNoMark arg)
             'f'  -> return (xs', Move arg)
             'p'  -> return (xs', SetPenWidth arg) -- default resets.
       -- Pen sizes grow and shrink by a scale factor.
       | x `elem` "'`\"" -> do
           (arg, xs') <- encodeArg xs $ FloatVar getPenScale
           case x of
             '\'' -> return (xs', ShrinkPen arg)
             '`'  -> return (xs', GrowPen arg)
             '"'  -> return (xs', SetPenScale arg)  -- default leave it alone.
       -- Things with angular dimensions
       | x `elem` "+-&^\\/" -> do
           (arg, xs') <- encodeArg xs $ FloatVar getAngle
           case x of
             '+'  -> return (xs', TurnLeft arg)  -- + z
             '-'  -> return (xs', TurnRight arg) -- - z
             '&'  -> return (xs', PitchDown arg) -- - y
             '^'  -> return (xs', PitchUp arg)   -- + y
             '\\' -> return (xs', RollLeft arg)  -- + x
             '/'  -> return (xs', RollRight arg) -- - x
       -- Things with string args
       | x == 'T' -> do
           (arg, xs') <- encodeStringArg xs $ StringVar getTexture
           return (xs', SetTexture arg)
       | x == 'C' -> do
           (arg, xs') <- encodeStringArg xs $ StringVar getColor
           return (xs', SetTexture arg)
       | x == 'O' -> do
           (arg, xs') <- encodeStringArg xs $ StringConst ""
           return (xs', PlaceObject arg)
       -- Macro invocation
       | x == '~'  ->  -- allow ~C for single and ~(foo) for long names
           let next = head xs in
           if null xs then throwE' $ "Dangling macro invocation " ++ show s
           else if next /= '(' then
             return (tail xs, InvokeMacro $ stringConst [next])
           else do
             (arg, xs') <- encodeStringArg xs $ StringConst ""
             return (xs', InvokeMacro arg)
       -- No arguments
       | x == '|'  -> return (xs, TurnAround)
       | x == '{'  -> return (xs, StartPolygon)
       | x == '.'  -> return (xs, MarkVertex)
       | x == '}'  -> return (xs, EndPolygon)
       | x == '$'  -> return (xs, ResetOrientation)
       | x == '@'  -> encodeMultiChar xs
       | otherwise -> return (xs, Noop)

encodeMultiChar :: (Turt a) => String -> ErrorM (String, TAction a)
encodeMultiChar [] = throwE' "Dangling multi-character."
encodeMultiChar (x:xs) =
  case x of
    'O' -> return (xs, DrawSphere)
    _   -> throwE' $ "Don't know what @" ++ [x] ++ " means."
  
encodeBranch :: (Turt a) => String -> ErrorM (String, TAction a)
encodeBranch s = do
  (rest, actions) <- encodeBranchRec s
  if null actions then
    return (rest, Noop)
  else
    return (rest, Branch actions)

-- Like encode actions, but expects a close bracket.
encodeBranchRec :: (Turt a) => String -> ErrorM (String, [TAction a])
encodeBranchRes [] = throwE' "Oops, branch ran off end."
encodeBranchRec s@(x:xs) =
  if x == ']' then return (xs, [])
  else do
    (rest, action) <- encodeAction s `amendE'` "encodeBranchRec"
    (rest', actions) <- encodeBranchRec rest
    return (rest', action : actions)

encodeArg :: (Turt a) => String -> FloatArg a -> ErrorM (FloatArg a, String)
encodeArg [] def = return (def, "")
encodeArg s@(x:xs) def
  | x /= '('  = return (def, s)
  | not (")" `isInfixOf` xs) =
    throwE' "No close parenthesis for action argument."
  | otherwise = do
    (expr, remaining) <- balancedSplit s
    case maybeRead expr of
      Just x -> return (FloatConst x, remaining)
      _      -> throwE' "Only accepting constant arguments for now."
  
encodeStringArg :: (Turt a) =>
  String -> StringArg a -> ErrorM (StringArg a, String)
encodeStringArg [] def = return (def, [])
encodeStringArg s@(x:xs) def
  | x /= '('  = return (def, s)
  | not (")" `isInfixOf` xs) =
    throwE' "No close parenthesis for action argument."
  | otherwise = do
    (expr, remaining) <- balancedSplit s
    return (StringConst expr, remaining)
{-
  fastFuncs['?']  = &Turtle::pushPoint;
  fastFuncs['#']  = &Turtle::popAndDrawLine;
-}
