module Value where

data Val = 
    Integer Integer
    | Tt
    | Ff
    deriving Show

value2Int :: Val -> Integer
value2Int (Integer x) = x
value2Int _ = error "value2Int: the value is not an Integer"

value2Str :: Val -> String
value2Str (Integer x) = show x
value2Str Tt = "True"
value2Str Ff = "False"