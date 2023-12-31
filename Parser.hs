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

-- if Bexp then [Stm] else [Stm] ;
parseStatements (T_if : tokens) = 
    case parseBody tokens of
        (condition, restTokens1) -> 
            -- error $ "\n\ncondition: " ++ show condition ++ "\n\n rest: " ++ show restTokens1
            case parseBody (restTokens1) of
                (trueBranch, restTokens2) ->
                    case parseBody (restTokens2) of
                        (falseBranch, restTokens3) ->
                            -- error $ "\n\ncondition: " ++ show condition ++ "\n\ntrueBranch: " ++ show (drop 1 trueBranch) ++ "\n\nfalseBranch: " ++ show (drop 1 falseBranch)
                            case restTokens3 of
                                (T_semicolon : restTokens4) ->
                                    S_if (parseB condition) (parseStatements (drop 1 trueBranch)) (parseStatements (drop 1 falseBranch)) : parseStatements restTokens4
                                (restTokens4) -> 
                                    S_if (parseB condition) (parseStatements (drop 1 trueBranch)) (parseStatements (drop 1 falseBranch)) : parseStatements restTokens4


parseBody :: [Token] -> ([Token], [Token])
parseBody (T_lbracket : tokens) = parseBodyAux tokens [] [T_lbracket]
parseBody tokens = (takeWhile (/= T_semicolon) tokens, drop 1 (dropWhile (/= T_semicolon) tokens))
-- parseBody tokens = (takeWhile (\t -> t /= T_semicolon && t /= T_then) tokens, drop 1 (dropWhile (\t -> t /= T_semicolon && t /= T_then) tokens))
-- parseBody tokens = (takeWhile (\t -> t /= T_semicolon && t /= T_then && t /= T_else) tokens, drop 1 (dropWhile (\t -> t /= T_semicolon && t /= T_then && t /= T_else) tokens))

parseBodyAux :: [Token] -> [Token] -> [Token] -> ([Token], [Token])
parseBodyAux [] body stack = error "Run-time error"
parseBodyAux (T_lbracket : tokens) body stack = parseBodyAux tokens (body ++ [T_lbracket]) (T_lbracket : stack)
parseBodyAux (T_rbracket : tokens) body (T_lbracket : stack) = 
    if stack == [] then (body, tokens)
    else parseBodyAux tokens (body ++ [T_rbracket]) stack
parseBodyAux (token : tokens) body stack = parseBodyAux tokens (body ++ [token]) stack

-- ----------------------------------------------------------------------------
parseA :: [Token] -> Aexp
parseA tokens =
    case parseSumOrSubOrProdOrIntOrVarOrPar tokens of
        Just (expr, []) -> expr
        _ -> error "Run-time error"

-- Int Var Par
parseIntOrVarOrPar :: [Token] -> Maybe (Aexp, [Token])
parseIntOrVarOrPar (T_var var : restTokens)
    = Just (A_var var, restTokens)
parseIntOrVarOrPar (T_integer num : restTokens)
    = Just (A_num num, restTokens)
parseIntOrVarOrPar (T_lbracket : restTokens1)
    = case parseSumOrSubOrProdOrIntOrVarOrPar restTokens1 of
        Just (expr, (T_rbracket : restTokens2)) ->
            Just (expr, restTokens2)
        Just _ -> Nothing -- no closing paren
        Nothing -> Nothing
parseIntOrVarOrPar tokens = Nothing

-- Prod Int Var Par
parseProdOrIntOrVarOrPar :: [Token] -> Maybe (Aexp, [Token])
parseProdOrIntOrVarOrPar tokens
    = case parseIntOrVarOrPar tokens of
        Just (expr1, (T_times : restTokens1)) ->
            case parseProdOrIntOrVarOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (A_mul expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result

-- Sum Sub Prod Int Var Par
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

-- ----------------------------------------------------------------------------
parseB :: [Token] -> Bexp
parseB tokens =
    case parseAndOrBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar tokens of
        Just (expr, []) -> expr
        _ -> error "Run-time error"

-- True False Par
parseTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseTrueOrFalseOrPar (T_bool True : restTokens)
    = Just (B_true, restTokens)
parseTrueOrFalseOrPar (T_bool False : restTokens)
    = Just (B_false, restTokens)
parseTrueOrFalseOrPar (T_lbracket : restTokens1)
    = case parseAndOrBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar restTokens1 of
        Just (expr, (T_rbracket : restTokens2)) ->
            Just (expr, restTokens2)
        Just _ -> Nothing -- no closing paren
        Nothing -> Nothing
parseTrueOrFalseOrPar tokens = Nothing

-- Leq True False Par
parseLeqOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseLeqOrTrueOrFalseOrPar tokens
    = case parseSumOrSubOrProdOrIntOrVarOrPar tokens of
        Just (expr1, (T_leq : restTokens1)) ->
            case parseSumOrSubOrProdOrIntOrVarOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (B_leq expr1 expr2, restTokens2)
                Nothing -> Nothing
        _ -> parseTrueOrFalseOrPar tokens

-- Aeq Leq True False Par
parseAeqOrLeqOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseAeqOrLeqOrTrueOrFalseOrPar tokens
    = case parseSumOrSubOrProdOrIntOrVarOrPar tokens of
        Just (expr1, (T_aeq : restTokens1)) ->
            case parseSumOrSubOrProdOrIntOrVarOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (B_aeq expr1 expr2, restTokens2)
                Nothing -> Nothing
        _ -> parseLeqOrTrueOrFalseOrPar tokens

-- Not Aeq Leq True False Par
-- parseNotOrAeqOrLeqOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
-- parseNotOrAeqOrLeqOrTrueOrFalseOrPar tokens
--     = case parseAeqOrLeqOrTrueOrFalseOrPar tokens of
--         Just (expr1, (T_not : restTokens1)) ->
--             case parseNotOrAeqOrLeqOrTrueOrFalseOrPar restTokens1 of
--                 Just (expr2, restTokens2) ->
--                     Just (B_not expr2, restTokens2)
--                 Nothing -> Nothing
--         a -> error $ "error: " ++ show a 
--         Just (expr1, _) -> error $ "Expr: " ++ show expr1
--         result -> result

parseNotOrAeqOrLeqOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseNotOrAeqOrLeqOrTrueOrFalseOrPar (T_not : restTokens)
    -- = error $ "rest: " ++ show restTokens
    = case parseAeqOrLeqOrTrueOrFalseOrPar restTokens of
        Just (expr1, restTokens1) ->
            Just (B_not expr1, restTokens1)
        -- result -> error "AMDIASJDIJA IODJAISO JDOAJD OIASJDIO JASSOIDJ AS"
        result -> parseAeqOrLeqOrTrueOrFalseOrPar restTokens
parseNotOrAeqOrLeqOrTrueOrFalseOrPar tokens = parseAeqOrLeqOrTrueOrFalseOrPar tokens


-- Beq Not Aeq Leq True False Par
parseBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar tokens
    = case parseNotOrAeqOrLeqOrTrueOrFalseOrPar tokens of
        Just (expr1, (T_beq : restTokens1)) ->
            case parseBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (B_beq expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result

-- And Beq Not Aeq Leq True False Par
parseAndOrBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseAndOrBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar tokens
    = case parseBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar tokens of 
        Just (expr1, (T_and : restTokens1)) ->
            case parseAndOrBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (B_and expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result