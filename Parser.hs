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

parse :: String -> Program
parse input = parseStatements (lexer input)

parseStatements :: [Token] -> [Stm]
parseStatements [] = []

parseStatements (T_var var : T_assign : tokens) =
    let 
        aexp = parseA (takeWhile (/= T_semicolon) tokens)
    in
        S_assign var aexp : parseStatements (drop 1 (dropWhile (/= T_semicolon) tokens))

-- parseStatements (T_while : tokens) = 
--     let 
--         bexp = parseB takeWhile (/= T_do) tokens
--         (body, tokens') = parseStatements drop 1 (dropWhile (/= T_do) tokens)
--     in
--         S_while bexp body : 

-- parseStatements (T_if _ tokens) = 
--     ...

-- ===============================<>=============================== --
-- %%=============================<>=============================%% --
-- %%%%===========================<>===========================%%%% --
-- %%%%%%%%=======================<>=======================%%%%%%%% --
-- %%%%%%%%%%%%%%%%===============<>===============%%%%%%%%%%%%%%%% --
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-<>-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% --
-- %%%%%%%%%%%%%%%%===============<>===============%%%%%%%%%%%%%%%% --
-- %%%%%%%%=======================<>=======================%%%%%%%% --
-- %%%%===========================<>===========================%%%% --
-- ===============================<>=============================== --

parseA :: [Token] -> Aexp
parseA tokens =
    case parseSumOrSubOrProdOrIntOrVarOrPar tokens of
        Just (expr, []) -> expr
        _ -> error "Parse error"

parseIntOrVar :: [Token] -> Maybe (Aexp, [Token])
parseIntOrVar (T_var var : restTokens)
    = Just (A_var var, restTokens)
parseIntOrVar (T_integer num : restTokens)
    = Just (A_num num, restTokens)
parseIntOrVar tokens
    = Nothing

parseProdOrIntOrVar :: [Token] -> Maybe (Aexp, [Token])
parseProdOrIntOrVar tokens
    = case parseIntOrVar tokens of
        Just (expr1, (T_times : restTokens1)) ->
            case parseProdOrIntOrVar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (A_mul expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result -- can be ’Nothing’ or valid

parseSumOrSubOrProdOrIntOrVar :: [Token] -> Maybe (Aexp, [Token])
parseSumOrSubOrProdOrIntOrVar tokens
    = case parseProdOrIntOrVar tokens of
        Just (expr1, (T_plus : restTokens1)) ->
            case parseProdOrIntOrVar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (A_add expr1 expr2, restTokens2)
                Nothing -> Nothing
        Just (expr1, (T_less : restTokens1)) ->
            case parseProdOrIntOrVar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (A_sub expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result -- could be ’Nothing’ or valid

parseIntOrVarOrParenExpr :: [Token] -> Maybe (Aexp, [Token])
parseIntOrVarOrParenExpr (T_var var : restTokens)
    = Just (A_var var, restTokens)
parseIntOrVarOrParenExpr (T_integer num : restTokens)
    = Just (A_num num, restTokens)
parseIntOrVarOrParenExpr (T_lbracket : restTokens1)
    = case parseSumOrSubOrProdOrIntOrVarOrPar restTokens1 of
        Just (expr, (T_rbracket : restTokens2)) ->
            Just (expr, restTokens2)
        Just _ -> Nothing -- no closing paren
        Nothing -> Nothing
parseIntOrVarOrParenExpr tokens = Nothing

parseProdOrIntOrVarOrPar :: [Token] -> Maybe (Aexp, [Token])
parseProdOrIntOrVarOrPar tokens
    = case parseIntOrVarOrParenExpr tokens of
        Just (expr1, (T_times : restTokens1)) ->
            case parseProdOrIntOrVarOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (A_mul expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result

parseSumOrSubOrProdOrIntOrVarOrPar::[Token] -> Maybe (Aexp, [Token])
parseSumOrSubOrProdOrIntOrVarOrPar tokens
    = case parseProdOrIntOrVarOrPar tokens of
        Just (expr1, (T_plus : restTokens1)) ->
            case parseSumOrSubOrProdOrIntOrVarOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (A_add expr1 expr2, restTokens2)
                Nothing -> Nothing
        Just (expr1, (T_less : restTokens1)) ->
            case parseSumOrSubOrProdOrIntOrVarOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (A_sub expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result

-- parseB :: [Token] -> (Bexp, [Token])
