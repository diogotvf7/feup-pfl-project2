module Parser where 

import Lexer

data Aexp
    = A_var String           -- Variable
    | A_num Integer          -- Integer constant
    | A_add Aexp Aexp        -- Addition
    | A_sub Aexp Aexp        -- Subtraction
    | A_mul Aexp Aexp        -- Multiplication
    deriving (Eq, Show)

data Bexp
    = B_true                 -- True
    | B_false                -- False
    | B_aeq Aexp Aexp        -- Arithmetic equality
    | B_beq Bexp Bexp        -- Boolean equality
    | B_leq Aexp Aexp        -- Less than or equal to
    | B_not Bexp             -- Logical NOT
    | B_and Bexp Bexp        -- Logical AND
    deriving (Eq, Show)

data Stm
    = S_assign String Aexp   -- Assignment statement
    | S_if Bexp [Stm] [Stm]  -- If statement with true and false branches
    | S_while Bexp [Stm]     -- While loop
    deriving (Eq, Show)

type Program = [Stm]

parse :: String -> [Stm]
parse input = parseStatements (lexer input)

-- Parse a list of statements
parseStatements :: [Token] -> [Stm]
parseStatements [] = []
parseStatements tokens =
    let (statement, restTokens) = parseStatement tokens
    in statement : parseStatements restTokens

-- Parse a single statement
parseStatement :: [Token] -> (Stm, [Token])
parseStatement tokens =
    case tokens of
        T_while : restTokens ->
            let (condition, tokensAfterWhile) = parseBexp restTokens
                (body, tokensAfterBody) = parseStatements tokensAfterWhile
            in (S_while condition body, tokensAfterBody)
        T_if : restTokens ->
            let (condition, tokensAfterIf) = parseBexp restTokens
                (trueBranch, tokensAfterTrue) = parseStatements tokensAfterIf
                (falseBranch, tokensAfterFalse) = parseStatements tokensAfterTrue
            in (S_if condition trueBranch falseBranch, tokensAfterFalse) -- falta o then else
        T_var varName : T_assign : restTokens ->
            let (expression, tokensAfterAssign) = parseAexp restTokens
            in (S_assign varName expression, tokensAfterAssign)
        -- Add cases for other statement types (if, assignment, etc.)
        _ -> error "Run-time error"

-- Parse a boolean expression
parseBexp :: [Token] -> (Bexp, [Token])
parseBexp tokens =
    -- Implement parsing of boolean expressions based on your language syntax
    -- Example: parse equality, parse logical AND, parse NOT, etc.
    -- Return the parsed boolean expression and the remaining tokens

