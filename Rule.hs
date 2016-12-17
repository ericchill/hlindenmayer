module Rule where
import RuleSpec
import Tape
import Data.List
import qualified Data.Map.Strict as Map
import Data.String.Utils

data LRule a = LRule (Map.Map (RuleSpec a) [[a]]) deriving (Eq, Show)

makeRule :: RuleSpec a -> [a] -> LRule a
makeRule spec production = LRule $ Map.singleton spec [production]

applyRule :: Eq a => LRule a -> Tape a -> Tape a -> Tape a
applyRule (LRule rules) tapeIn tapeOut =
  let matches = Map.filterWithKey (\spec _ -> matchSpec spec tapeIn) rules
      (_, productions) = Map.findMax matches
  in
    appendHead (head productions) tapeOut  -- later pick one randomly

addSuccessor :: Eq a => (RuleSpec a, [a]) -> LRule a -> LRule a
addSuccessor (spec, prod) (LRule rules) =
  case Map.lookup spec rules of
    Just productions -> LRule $ Map.insert spec (prod:productions) rules
    _ -> LRule $ Map.singleton spec [prod]

-- Parse texty L-system with usual representation.
parseRule :: Metagrammar Char -> [Char] -> (RuleSpec Char, [Char])
parseRule meta input =
  (parseRuleSpec meta specStr, strip prodStr)
  where sides = split "->" input
        specStr = sides !! 0
        prodStr = sides !! 1

