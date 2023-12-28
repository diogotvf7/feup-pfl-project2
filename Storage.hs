module Storage where

import Data.List (intercalate)
import qualified Data.Map as Map
import Value

type Var = String

type Storage = Map.Map Var Val

createEmptyStorage :: Storage
createEmptyStorage = Map.empty

push :: Storage -> Var -> Val -> Storage
push storage var val = Map.insert var val storage

get :: Storage -> Var -> Maybe Val
get storage var = Map.lookup var storage

storage2Str :: Storage -> String
storage2Str storage
  | Map.null storage = ""
  | otherwise = "[" ++ intercalate ", " (map entryToStr (Map.toList storage)) ++ "]"

entryToStr :: (Var, Val) -> String
entryToStr (var, val) = var ++ " = " ++ value2Str val