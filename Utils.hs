module Utils (
  ErrorM(..),
  BoolMonad(..),
  caseM,
  (&&&&), (||||), lNot,
  module Error,
  module Control.Applicative,
  )
  where
import Error
import Control.Applicative

{-
> caseM [(m Bool, a)] default
-}
caseM :: [(ErrorM Bool, ErrorM a)] -> ErrorM a -> ErrorM a
caseM [] def = def
caseM (c:cs) def =
  let (cond, action) = c in
    cond >>= (\test ->
                if test then action
                else caseM cs def)

(&&&&) :: (Applicative m) => m Bool -> m Bool -> m Bool
(&&&&) = liftA2 (&&)

(||||) :: (Applicative m) => m Bool -> m Bool -> m Bool
(||||) = liftA2 (||)

lNot :: (Applicative m) => m Bool -> m Bool
lNot = liftA not
