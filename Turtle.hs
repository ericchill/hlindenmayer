{-# LANGUAGE FlexibleContexts #-}
module Turtle (
  TAction(..),
  Turt(..),
  encodeActions,
  foldActions,
  TPosition,
  TOrientation,
  EuclideanState,
  )
where
import Math
import Options
import Utils
import Control.Monad
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
  Noop deriving (Show)

encodeActions :: (Turt a) => String -> ErrorM [TAction a]
encodeActions s =
  do
    (_, actions) <- appendErrorT " in encodeActions" (encodeActionsRec s)
    return actions

encodeActionsRec :: (Turt a) => String -> ErrorM (String, [TAction a])
encodeActionsRec [] =
  do
    (_, action) <- encodeAction []
    return ([], [action])
encodeActionsRec s =
  do
    (rest, action) <- encodeAction s
    (rest', actions) <- encodeActionsRec rest
    return (rest', action : actions)

-- On error string is at error, otherwise it's after parsed action.
-- Likely cause of future errors will be in parsing expressions as arguments.
encodeAction :: (Turt a) => String -> ErrorM (String, TAction a)
encodeAction [] = return ([], Noop)
encodeAction s@(x:xs)
  | x == '['  = encodeBranch xs
  | x == 'F'  = return (xs, DrawLine $ floatConst 1)
  | x == 'G'  = return (xs, DrawNoMark $ floatConst 1)
  | x == 'f'  = return (xs, Move $ floatConst 1)
  | x == '+'  = return (xs, TurnLeft getAngle)  -- + z
  | x == '-'  = return (xs, TurnRight getAngle) -- - z
  | x == '&'  = return (xs, PitchDown getAngle) -- - y
  | x == '^'  = return (xs, PitchUp getAngle)   -- y
  | x == '\\' = return (xs, RollLeft getAngle)  -- - x
  | x == '/'  = return (xs, RollRight getAngle) -- x
  | x == '|'  = return (xs, TurnAround)
  | x == '$'  = return (xs, ResetOrientation)
  | x == '~'  =
      if null xs then
        throwE $ "Dangling macro invocation " ++ s
      else
        return (tail xs, InvokeMacro $ stringConst [head xs])
  | x == '\'' = return (xs, ShrinkPen $ floatConst 1.2)
  | x == '`'  = return (xs, GrowPen $ floatConst 1.2)
  | x == 'p'  = return (xs, SetPenWidth $ floatConst 1)
  | otherwise = return (xs, Noop)

encodeBranch :: (Turt a) => String -> ErrorM (String, TAction a)
encodeBranch s =
  do
    (rest, actions) <- encodeBranchRec s
    return (rest, Branch actions)

-- Like encode actions, but expects a close bracket.
encodeBranchRec :: (Turt a) => String -> ErrorM (String, [TAction a])
encodeBranchRes [] = throwE "Oops, branch ran off end."
encodeBranchRec s@(x:xs)
  | x == ']'  = return (xs, [])
  | otherwise =
    do
      (rest, action) <- appendErrorT " in encodeBranchRec" (encodeAction s)
      (rest', actions) <- encodeBranchRec rest
      return (rest', action : actions)

type TurtleMonad a = ExceptT String IO a

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

  getMacro :: a -> StringArg a -> [TAction a]
  getOpt   :: (Read b) => a -> String -> b -> ErrorM b
  getDoubleOpt   :: a -> String -> Double -> ErrorM Double

  getAngle :: FloatArg a
  getAngle = FloatVar (\t -> do
                          val <- getDoubleOpt t "delta" 90.0
                          return $ val * pi / 180.0)
  
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
  rollLeft tur = reorientMinus tur xAxis

  rollRight :: a -> FloatArg a -> TurtleMonad a
  rollRight tur = reorient tur xAxis

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
  invokeMacro tur arg = foldActions (getMacro tur arg) tur


foldActions :: (Turt a) => [TAction a] -> a -> TurtleMonad a
foldActions [] t = return t
foldActions (x:xs) t = doAction t x >>= foldActions xs

data EuclideanState a = EuclideanState {
  tPosition    :: TPosition,
  tOrientation :: TOrientation
  }

{-
  fastFuncs['[']  = &Turtle::pushTurtle;
  fastFuncs['.']  = &Turtle::markVertex;
  fastFuncs['{']  = &Turtle::startPolygon;
  fastFuncs['?']  = &Turtle::pushPoint;
  fastFuncs['#']  = &Turtle::popAndDrawLine;
-}

