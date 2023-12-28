module State where

import Data.List (intercalate)
import qualified Data.Map as Map
import Value

type Var = String

type State = Map.Map Var Val

createEmptyState :: State
createEmptyState = Map.empty

push :: State -> Var -> Val -> State
push state var val = Map.insert var val state

get :: State -> Var -> Maybe Val
get state var = Map.lookup var state

state2Str :: State -> String
state2Str state
  | Map.null state = ""
  | otherwise = intercalate "," (map entryToStr (Map.toList state))

entryToStr :: (Var, Val) -> String
entryToStr (var, val) = var ++ "=" ++ value2Str val