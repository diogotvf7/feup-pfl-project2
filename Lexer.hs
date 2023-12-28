data Token
    = PushToken 
    | AddToken
    | MulToken
    | SubToken
    | TrueToken
    | FalseToken
    | EqToken
    | LeToken
    | AndToken
    | NegToken
    | FetchToken
    | StoreToken
    | NoopToken
    | BranchToken
    | LoopToken
    | IntToken
    deriving (Show)

-- lexer :: String -> [Token]
-- lexer [] = []
-- lexer (’+’ : restStr) = PlusTok : lexer restStr
-- lexer (’*’ : restStr) = TimesTok : lexer restStr
-- lexer (’(’ : restStr) = OpenP : lexer restStr
-- lexer (’)’ : restStr) = CloseP : lexer restStr
-- lexer (chr : restStr)
-- | isSpace chr = lexer restStr