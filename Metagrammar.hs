module Metagrammar (
  Metagrammar(..)
  , mSetIgnore
  , testMeta
  )
where
import Utils
import Tape
import Control.Applicative
import Control.Monad

data Metagrammar = Metagrammar {
  isIgnored      :: Char -> Bool,
  isWild         :: Char -> Bool,
  isBreak        :: Char -> Bool,
  rsSig          :: String
  }

instance Eq Metagrammar where
  a == b = rsSig a == rsSig b

instance Show Metagrammar where
  show a = "(Metagrammar " ++ show (rsSig a) ++ ")"

mSetIgnore :: String -> Metagrammar -> Metagrammar
mSetIgnore a m = m { isIgnored = (`elem` a) }

testMeta :: Metagrammar
testMeta =
  let cf = const False
      cf2 = const (const False)
  in
  Metagrammar {
    isIgnored      = cf,
    isWild         = cf,
    isBreak        = cf,
    rsSig          = "test"
    }

  
