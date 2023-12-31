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

data Exp = Aexp | Bexp

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

parseA :: [Token] -> Aexp
parseA tokens =
    case parseSumOrSubOrProdOrIntOrVarOrPar tokens of
        Just (expr, []) -> expr
        _ -> error "Parse error"

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
-- Reconheccer que voltamos a uma expressão booleana (????)
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

parseB :: [Token] -> Bexp
parseB tokens =
    case parseAndOrBeqOrLeqOrAeqOrNotOrTrueOrFalseOrPar tokens of
        Just (expr, []) -> expr
        _ -> error "Parse error"

-- True False ()
parseTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseTrueOrFalseOrPar (T_bool True : restTokens)
    = Just (B_true, restTokens)
parseTrueOrFalseOrPar (T_bool False : restTokens)
    = Just (B_false, restTokens)
parseTrueOrFalseOrPar (T_lbracket : restTokens1)
    = case parseAndOrBeqOrLeqOrAeqOrNotOrTrueOrFalseOrPar restTokens1 of
        Just (expr, (T_rbracket : restTokens2)) ->
            Just (expr, restTokens2)
        Just _ -> Nothing -- no closing paren
        Nothing -> Nothing
parseTrueOrFalseOrPar tokens = Nothing

-- Not True False ()
parseNotOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseNotOrTrueOrFalseOrPar tokens
    = case parseTrueOrFalseOrPar tokens of
        Just (expr, restTokens) -> Just (expr, restTokens)
        Nothing ->
            case parseNotOrTrueOrFalseOrPar tokens of
                Just (expr, (T_not : restTokens)) ->
                    Just (B_not expr, restTokens)
                result -> result

-- Leq Aeq not True False ()
parseLeqOrAeqOrNotOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseLeqOrAeqOrNotOrTrueOrFalseOrPar tokens 
    = case parseNotOrTrueOrFalseOrPar tokens of
        Just (expr1, (T_leq : restTokens1)) ->
            case parseLeqOrAeqOrNotOrTrueOrFalseOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (B_leq expr1 expr2, restTokens2)
                Nothing -> Nothing
        Just (expr1, (T_aeq : restTokens1)) ->
            case parseLeqOrAeqOrNotOrTrueOrFalseOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (B_aeq expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result

-- parseLeqOrAeqOrNotOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
-- parseLeqOrAeqOrNotOrTrueOrFalseOrPar tokens 
--     = case parseNotOrTrueOrFalseOrPar tokens of
--         Just (expr1, (T_aeq : restTokens1)) ->
--             case parseArithmeticExpr restTokens1 of
--                 Just (expr2, restTokens2) ->
--                     Just (B_aeq expr1 expr2, restTokens2)
--                 Nothing -> Nothing
--         Just (expr1, (T_leq : restTokens1)) ->
--             case parseArithmeticExpr restTokens1 of
--                 Just (expr2, restTokens2) ->
--                     Just (B_leq expr1 expr2, restTokens2)
--                 Nothing -> Nothing
--         result -> result

-- And Beq Leq Aeq not True False ()
parseAndOrBeqOrLeqOrAeqOrNotOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseAndOrBeqOrLeqOrAeqOrNotOrTrueOrFalseOrPar tokens
    = case parseLeqOrAeqOrNotOrTrueOrFalseOrPar tokens of 
        Just (expr1, (T_and : restTokens1)) ->
            case parseAndOrBeqOrLeqOrAeqOrNotOrTrueOrFalseOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (B_and expr1 expr2, restTokens2)
                Nothing -> Nothing
        Just (expr1, (T_beq : restTokens1)) ->
            case parseAndOrBeqOrLeqOrAeqOrNotOrTrueOrFalseOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (B_beq expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result

-- parseArithmeticExpr :: [Token] -> Maybe (Aexp, [Token])
-- parseArithmeticExpr tokens
--     = case parseSumOrSubOrProdOrIntOrVarOrPar tokens of
--         Just (expr, restTokens) -> Just (expr, restTokens)
--         _ -> Nothing