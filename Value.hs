module Value where

-- Definition of a custom data type 'Val' with three possible values (Integer, True and False).
data Val = 
    Integer Integer
    | Tt
    | Ff
    deriving (Eq, Show)

-- Function to convert a 'Val' (value) to an Integer.
value2Int :: Val -> Integer
value2Int (Integer x) = x
value2Int _ = error "Run-time error"


-- Function to convert a 'Val' to a String.
value2Str :: Val -> String
value2Str (Integer x) = show x
value2Str Tt = "True"
value2Str Ff = "False"