module Tape (
  Tape(..),
  newTape,
  atEnd,
  atStart,
  rewind,
  moveRight,
  moveRightBy,
  moveRightMatching,
  moveLeft,
  tapeHead,
  appendHead
  )
where
import Utils
import qualified Data.List as List
--(foldl', foldr)

-- First item is reverse of first half.
data Tape a = Tape {
  leftPart :: [a],
  rightPart  :: [a]
  } deriving (Eq)

instance Foldable Tape where
  foldr f acc tape = List.foldr f acc $ (tapeHead . rewind) tape

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

rewind :: Tape a -> Tape a
rewind (Tape l r) = Tape [] $ reverse l ++ r

moveRight :: Tape a -> Tape a
moveRight (Tape l (x:xs)) = Tape (x:l) xs
moveRight (Tape _ []) = error "Tape already at right."

moveRightBy :: Tape a -> Int -> Tape a
moveRightBy tape dist = List.foldl' (\t _ -> moveRight t) tape [1..dist]

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
appendHead (Tape l r) list = Tape (foldr (:) l list) r
