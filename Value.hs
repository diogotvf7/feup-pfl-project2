module Value where

data Val = 
    Integer Integer
    | Tt
    | Ff
    deriving Show

value2Str :: Val -> String
value2Str (Integer x) = show x
value2Str Tt = "True"
value2Str Ff = "False"