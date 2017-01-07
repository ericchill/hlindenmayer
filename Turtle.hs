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
import Math
import Options
import Utils
import Control.Monad
import Data.Strict.Tuple
import Data.String.Utils
import Linear.V3
import System.IO

type TPosition = V3F
type TOrientation = M33F

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
  InvokeMacro (StringArg a) |
  SetTexture (StringArg a) |
  PlaceObject (StringArg a) |
  Noop deriving (Show)

encodeActions :: (Turt a) => String -> ErrorM [TAction a]
encodeActions s =
  do
    (_, actions) <- appendErrorT " in encodeActions" (encodeActionsRec (Utils.trace s s))
    return actions

encodeActionsRec :: (Turt a) => String -> ErrorM (String, [TAction a])
encodeActionsRec [] =
  do
    (_, action) <- encodeAction []
    return ([], [action])
encodeActionsRec s =
  do
    (rest, action) <- encodeAction s
    (rest', actions) <- encodeActionsRec $! rest
    return (rest', (:) action $! actions)

-- On error string is at error, otherwise it's after parsed action.
-- Likely cause of future errors will be in parsing expressions as arguments.
encodeAction :: (Turt a) => String -> ErrorM (String, TAction a)
encodeAction [] = return ([], Noop)
encodeAction s@(x:xs)
  | x == '['  = encodeBranch xs
  | x `elem` "FGfp" = do
      -- length dimension
      (arg, xs') <- encodeArg xs (return . const 1.0)
      let farg = FloatVar arg
      case x of
        'F'  -> return (xs', DrawLine farg)
        'G'  -> return (xs', DrawNoMark farg)
        'f'  -> return (xs', Move farg)
        'p'  -> return (xs', SetPenWidth farg)
  | x `elem` "'`" = do
      -- pen size ratios
      (arg, xs') <- encodeArg xs (return . const 1.1)
      let farg = FloatVar arg
      case x of
        '\'' -> return (xs', ShrinkPen farg)
        '`'  -> return (xs', GrowPen farg)
  | x `elem` "+-&^\\/" = do
      -- angle dimension
      (arg, xs') <- encodeArg xs getAngle
      let farg = FloatVar arg
      case x of
        '+'  -> return (xs', TurnLeft farg)  -- + z
        '-'  -> return (xs', TurnRight farg) -- - z
        '&'  -> return (xs', PitchDown farg) -- - y
        '^'  -> return (xs', PitchUp farg)   -- + y
        '\\' -> return (xs', RollLeft farg)  -- + x
        '/'  -> return (xs', RollRight farg) -- - x
  | x == '|'  = return (xs, TurnAround)
  | x == '$'  = return (xs, ResetOrientation)
  | x == '~'  =  -- allow ~C for single and ~(foo) for long names
      if null xs then
        throwE' $ "Dangling macro invocation " ++ s
      else if x /= '(' then
        return (tail xs, InvokeMacro $ stringConst [head xs])
      else do
        (arg, xs') <- encodeStringArg xs (return . const "")
        return (xs', InvokeMacro $ StringVar arg)
  | x `elem` "OT" = do
      (arg, xs') <- encodeStringArg xs (return . const "")
      let sarg = StringVar arg in
        case x of
          'O' -> return (xs', PlaceObject sarg)
          'T' -> return (xs', SetTexture sarg)
  | otherwise = return (xs, Noop)

encodeBranch :: (Turt a) => String -> ErrorM (String, TAction a)
encodeBranch s =
  do
    (rest, actions) <- encodeBranchRec s
    return (rest, Branch $! actions)

-- Like encode actions, but expects a close bracket.
encodeBranchRec :: (Turt a) => String -> ErrorM (String, [TAction a])
encodeBranchRes [] = throwE' "Oops, branch ran off end."
encodeBranchRec s@(x:xs)
  | x == ']'  = return (xs, [])
  | otherwise =
    do
      (rest, action) <- appendErrorT " in encodeBranchRec" (encodeAction s)
      (rest', actions) <- encodeBranchRec rest
      return (rest', action : actions)

encodeArg :: (Read a, Turt b) => String -> (b -> ErrorM a) -> ErrorM (b -> ErrorM a, String)
encodeArg [] def = return (def, [])
encodeArg s@(x:xs) def
  | x /= '('  = return (def, s)
  | otherwise =
    if ')' `notElem` xs then throwE' "No close parenthesis for action argument."
    else do
      (expr, remaining) <- balancedSplit s
      case maybeRead expr of
        Just x -> return (const x, remaining)
        _      -> return (evalArgExpr expr, remaining)
  
encodeStringArg :: (Turt a) =>
  String -> (a -> ErrorM String) -> ErrorM (a -> ErrorM String, String)
encodeStringArg [] def = return (def, [])
encodeStringArg s@(x:xs) def
  | x /= '('  = return (def, s)
  | otherwise =
    if ')' `notElem` xs then throwE' "No close parenthesis for action argument."
    else do
      (expr, remaining) <- balancedSplit s
      return (return . const expr, remaining)
  
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
  getPenWidth      :: a -> Double
  setPenWidth      :: a -> FloatArg a -> TurtleMonad a

  getMacro     :: a -> StringArg a -> ErrorM [TAction a]
  getOpt       :: (Read b) => a -> String -> b -> ErrorM b

  evalArgExpr  :: (Read b) => String -> a -> ErrorM b

  getAngle :: a -> ErrorM Double
  getAngle t = do
    val <- getOpt t "delta" 90.0
    return $ val * pi / 180.0
  
  doAction :: a -> TAction a -> TurtleMonad a
  doAction t (Branch actions) = foldActions actions t >> return t
  doAction t (DrawLine dt)    = drawLine t dt
  doAction t (DrawNoMark dt)  = drawLine t dt
  doAction t (Move dt)        = move t dt
  doAction t (TurnLeft da)    = turnLeft t da
  doAction t (TurnRight da)   = turnRight t da
  doAction t (PitchDown da)   = pitchDown t da
  doAction t (PitchUp da)     = pitchUp t da
  doAction t (RollLeft da)    = rollLeft t da
  doAction t (RollRight da)   = rollRight t da
  doAction t TurnAround       = turnAround t
  doAction t ResetOrientation = resetOrientation t
  doAction t (ShrinkPen a)    = shrinkPen t a
  doAction t (GrowPen a)      = growPen t a
  doAction t (SetPenWidth a)  = setPenWidth t a
  doAction t (InvokeMacro a)  = invokeMacro t a
  doAction t (SetTexture a)   = setTexture t a
  doAction t _ = return t

  reorient :: a -> V3F -> FloatArg a -> TurtleMonad a
  reorient tur axis arg = do
    angle <- mapErrorM $ getFloatArg arg tur
    setOrientation tur $ rotateMatrix (getOrientation tur) axis angle

  reorientMinus :: a -> V3F -> FloatArg a -> TurtleMonad a
  reorientMinus tur axis arg = do
    angle <- mapErrorM $ getFloatArg arg tur
    setOrientation tur
      $ rotateMatrix (getOrientation tur) axis (- angle)
    
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

  setTexture :: a -> StringArg a -> TurtleMonad a
  
foldActions :: (Turt a) => [TAction a] -> a -> TurtleMonad a
foldActions [] t = return t
foldActions (a:as) t = do
  t' <- doAction t a
  foldActions as $! t'

{-
  fastFuncs['[']  = &Turtle::pushTurtle;
  fastFuncs['.']  = &Turtle::markVertex;
  fastFuncs['{']  = &Turtle::startPolygon;
  fastFuncs['?']  = &Turtle::pushPoint;
  fastFuncs['#']  = &Turtle::popAndDrawLine;
-}
