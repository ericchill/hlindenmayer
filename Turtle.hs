module Turtle (
  TAction,
  encodeActions,
  foldActions,
  FloatArg(..),
  floatConst,
  StringArg(..),
  stringConst,
  TPosition,
  TOrientation,
  Turt(..),
  EuclideanState,
  )
where
import Math
import Control.Monad
import Data.Either.Unwrap (fromRight)
import Linear.V3
import System.IO

type TPosition = V3F
type TOrientation = M33F

data FloatArg a = FloatArg (a -> Float)
data StringArg a = StringArg (a -> String)

floatConst :: Turt a => Float -> FloatArg a
floatConst val = FloatArg $ const val

stringConst :: Turt a => String -> StringArg a
stringConst val = StringArg $ const val

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
  Noop

type EncodeResult a = Either (String, String) (String, TAction a)

encodeActions :: Turt a => String -> Either String [TAction a]
encodeActions [] = Right [(snd . fromRight) $ encodeAction []]
encodeActions actions =
  case encodeAction actions of
    Right (rest, enc) ->
      case encodeActions rest of
        Right encs -> Right (enc : encs)
        err        -> err
    Left (_, err) -> Left err

-- On error string is at error, otherwise it's after parsed action.
-- Likely cause of future errors will be in parsing expressions as arguments.
encodeAction :: Turt a => String -> EncodeResult a
encodeAction [] = Right ([], Noop)
encodeAction s@(x:xs)
  | x == '['  = encodeBranch xs
  | x == 'F'  = Right (xs, DrawLine $ floatConst 1)
  | x == 'G'  = Right (xs, DrawNoMark $ floatConst 1)
  | x == 'f'  = Right (xs, Move $ floatConst 1)
  | x == '+'  = Right (xs, TurnLeft $ floatConst 1)
  | x == '-'  = Right (xs, TurnRight $ floatConst 1)
  | x == '&'  = Right (xs, PitchDown $ floatConst 1)
  | x == '^'  = Right (xs, PitchUp $ floatConst 1)
  | x == '\\' = Right (xs, RollLeft $ floatConst 1)
  | x == '/'  = Right (xs, RollRight $ floatConst 1)
  | x == '|'  = Right (xs, TurnAround)
  | x == '$'  = Right (xs, ResetOrientation)
  | x == '~'  =
    if null xs then
      Left (s, "Dangling macro invocation.")
    else
      Right (tail xs, InvokeMacro $ stringConst [head xs])
  | x == '\'' = Right (xs, ShrinkPen $ floatConst 1)
  | x == '`'  = Right (xs, GrowPen $ floatConst 1)
  | x == 'p'  = Right (xs, SetPenWidth $ floatConst 1)
  | otherwise = Right (xs, Noop)

encodeBranch :: Turt a => String -> EncodeResult a
encodeBranch s =
  case encodeBranchRec s of
    Right (rest, actions) -> Right (rest, Branch actions)
    Left err              -> Left err

encodeBranchRec :: Turt a => String -> Either (String, String) (String, [TAction a])
encodeBranchRes [] = Left ([], "Oops, branch ran off end.")
encodeBranchRec s@(x:xs)
  | x == ']'  = Right (xs, [])
  | otherwise =
    case encodeAction s of
      Right (rest, action) ->
        case encodeBranchRec rest of
          Right (rest', actions) -> Right (rest', action : actions)
          err                    -> err
      Left err -> Left err
      

class Turt a where
  drawLine   :: a -> FloatArg a -> IO a
  drawNoMark :: a -> FloatArg a -> IO a
  move       :: a -> FloatArg a -> IO a

  getPos           :: a -> TPosition
  setPos           :: a -> TPosition -> IO a
  getOrientation   :: a -> TOrientation
  setOrientation   :: a -> TOrientation -> IO a
  resetOrientation :: a -> IO a
  getPenWidth      :: a -> Float
  setPenWidth      :: a -> FloatArg a -> IO a

  getMacro         :: a -> StringArg a -> [TAction a]
  
  doAction :: a -> TAction a -> IO a
  doAction t (Branch actions) = foldActions t actions
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

  reorient :: a -> V3F -> FloatArg a -> IO a
  reorient tur axis (FloatArg arg) =
    setOrientation tur $ rotateMatrix (getOrientation tur) axis $ arg tur

  reorientMinus :: a -> V3F -> FloatArg a -> IO a
  reorientMinus tur axis (FloatArg arg) =
    setOrientation tur $ rotateMatrix (getOrientation tur) axis $ - (arg tur)
    
  turnLeft :: a -> FloatArg a -> IO a
  turnLeft tur = reorient tur zAxis

  turnRight :: a -> FloatArg a -> IO a
  turnRight tur = reorientMinus tur zAxis

  pitchDown :: a -> FloatArg a -> IO a
  pitchDown tur = reorient tur yAxis

  pitchUp :: a -> FloatArg a -> IO a
  pitchUp tur = reorient tur yAxis

  rollLeft :: a -> FloatArg a -> IO a
  rollLeft tur = reorientMinus tur xAxis

  rollRight :: a -> FloatArg a -> IO a
  rollRight tur = reorient tur xAxis

  turnAround :: a -> IO a
  turnAround tur = reorient tur zAxis $ floatConst pi

  shrinkPen :: a -> FloatArg a -> IO a
  shrinkPen tur (FloatArg arg) = setPenWidth tur $ floatConst $ getPenWidth tur - arg tur
  
  growPen :: a -> FloatArg a -> IO a
  growPen tur (FloatArg arg) = setPenWidth tur $ floatConst $ getPenWidth tur + arg tur

  invokeMacro :: a -> StringArg a -> IO a
  invokeMacro tur arg = foldActions tur $ getMacro tur arg
  
foldActions :: Turt a => a -> [TAction a] -> IO a
foldActions = foldM doAction


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

