module Tape (
  Tape(..),
  newTape,
  atEnd,
  atStart,
  moveRight,
  moveRightBy,
  moveRightMatching,
  moveLeft,
  tapeHead,
  appendHead
  )
where
import Utils
import Data.List (foldl', foldr)

-- First item is reverse of first half.
data Tape a = Tape {
  leftPart :: [a],
  rightPart  :: [a]
  } deriving (Eq)

instance Show a => Show (Tape a) where
  show (Tape l r) =
   "(Tape " ++ show (reverse $ take 10 l) ++ " " ++ show (take 10 r) ++ ")"

newtype Productions a = Productions { runProductions :: a }

newTape :: [a] -> Tape a
newTape = Tape []

atEnd :: Tape a -> Bool
atEnd (Tape _ []) = True
atEnd _           = False

atStart :: Tape a -> Bool
atStart (Tape [] _) = True
atStart _           = False

moveRight :: Tape a -> Tape a
moveRight (Tape l (x:xs)) = Tape (x:l) xs
moveRight (Tape _ []) = error "Tape already at right."

moveRightBy :: Tape a -> Int -> Tape a
moveRightBy tape dist = foldl' (\t _ -> moveRight t) tape [1..dist]

moveRightMatching :: (Eq a) => [a] -> Tape a -> Tape a
moveRightMatching x t@(Tape _ r) =
  moveRightBy t $ length $ takeWhile (uncurry (==)) $ zip x r

moveLeft :: Tape a -> Tape a
moveLeft (Tape (x:xs) r) = Tape xs (x:r)
moveLeft (Tape [] _) = error "Tape already at left."

tapeHead :: Tape a -> [a]
tapeHead (Tape _ r) = r

tapeHeadLeft :: Tape a -> [a]
tapeHeadLeft (Tape l _) = reverse l

-- Place list onto tape, leaving head just past last element
appendHead :: Show a => Tape a -> [a] -> Tape a
appendHead t@(Tape l r) list =
  let newLeft = (reverse list) ++ l
      result = Tape newLeft r
  in
    traceIf False (
    "appending " ++ show list ++ " to " ++ (show t) ++ " yielding " ++ show result ++ "(newLeft = " ++ show newLeft ++ ")")
    result
--appendHead list (Tape l r) = Tape ((foldr (:) l) $ trace ("appending " ++ show list) list) r
