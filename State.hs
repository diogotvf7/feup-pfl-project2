module State where

import Data.List (intercalate)
import qualified Data.Map as Map
import Value

type Var = String

type State = Map.Map Var Val

-- Function to create an empty state.
createEmptyState :: State
createEmptyState = Map.empty

-- Function to push a variable-value pair into the state.
push :: State -> Var -> Val -> State
push state var val = Map.insert var val state

-- Function to get the value of a variable from the state.
get :: State -> Var -> Maybe Val
get state var = Map.lookup var state

-- Function to convert a state to a string.
state2Str :: State -> String
state2Str state
  | Map.null state = ""
  | otherwise = intercalate "," (map entryToStr (Map.toList state))

-- Function to convert a variable-value pair to a string.
entryToStr :: (Var, Val) -> String
entryToStr (var, val) = var ++ "=" ++ value2Str val