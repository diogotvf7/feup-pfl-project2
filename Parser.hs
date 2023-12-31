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

parseStatements (T_if : tokens) = 
    let 
        (bexp, tokens') = getBody tokens
        ()

getBody :: [Token] -> ([Token], [Token])
getBody (T_lbracket : tokens) = getBodyAux tokens [] [T_lbracket]
getBody tokens = (takeWhile (/= T_semicolon), drop 1 (dropWhile (/= T_semicolon) tokens))

getBodyAux :: [Token] -> [Token] -> [Token] -> ([Token], [Token])
getBodyAux [] body stack = error "Run-time error"
getBodyAux (T_lbracket : tokens) body stack = getBodyAux tokens (body ++ [T_lbracket]) (T_lbracket : stack)
getBodyAux (T_rbracket : tokens) body (T_lbracket : stack) = 
    if stack == [] then (body, tokens)
    else getBodyAux tokens (body ++ [T_rbracket]) stack
getBodyAux (token : tokens) body stack = getBodyAux tokens (body ++ [token]) stack

parseA :: [Token] -> Aexp
parseA tokens =
    case parseSumOrSubOrProdOrPar tokens of
        Just (expr, []) -> expr
        _ -> error "Run-time error"

-- Int Var Par
parseIntOrVarOrPar :: [Token] -> Maybe (Aexp, [Token])
parseIntOrVarOrPar (T_var var : restTokens)
    = Just (A_var var, restTokens)
parseIntOrVarOrPar (T_integer num : restTokens)
    = Just (A_num num, restTokens)
parseIntOrVarOrPar (T_lbracket : restTokens1)
    = case parseSumOrSubOrProdOrPar restTokens1 of
        Just (expr, (T_rbracket : restTokens2)) ->
            Just (expr, restTokens2)
        Just _ -> Nothing -- no closing paren
        Nothing -> Nothing
parseIntOrVarOrPar tokens = Nothing

-- Prod Int Var Par
parseProdOrPar :: [Token] -> Maybe (Aexp, [Token])
parseProdOrPar tokens
    = case parseIntOrVarOrPar tokens of
        Just (expr1, (T_times : restTokens1)) ->
            case parseProdOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (A_mul expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result

-- Sum Sub Prod Int Var Par
parseSumOrSubOrProdOrPar::[Token] -> Maybe (Aexp, [Token])
parseSumOrSubOrProdOrPar tokens
    = case parseProdOrPar tokens of
        Just (expr1, (T_plus : restTokens1)) ->
            case parseSumOrSubOrProdOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (A_add expr1 expr2, restTokens2)
                Nothing -> Nothing
        Just (expr1, (T_less : restTokens1)) ->
            case parseSumOrSubOrProdOrPar restTokens1 of
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
    = case parseSumOrSubOrProdOrPar tokens of
        Just (expr1, (T_leq : restTokens1)) ->
            case parseSumOrSubOrProdOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (B_leq expr1 expr2, restTokens2)
                Nothing -> Nothing
        _ -> parseTrueOrFalseOrPar tokens

-- Aeq Leq True False Par
parseAeqOrLeqOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseAeqOrLeqOrTrueOrFalseOrPar tokens
    = case parseSumOrSubOrProdOrPar tokens of
        Just (expr1, (T_leq : restTokens1)) ->
            case parseSumOrSubOrProdOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (B_leq expr1 expr2, restTokens2)
                Nothing -> Nothing
        _ -> parseLeqOrTrueOrFalseOrPar tokens

-- Not Aeq Leq True False Par
parseNotOrAeqOrLeqOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseNotOrAeqOrLeqOrTrueOrFalseOrPar tokens
    = case parseAeqOrLeqOrTrueOrFalseOrPar tokens of
        Just (expr, restTokens) -> Just (expr, restTokens)
        Nothing ->
            case parseNotOrAeqOrLeqOrTrueOrFalseOrPar tokens of
                Just (expr, (T_not : restTokens)) ->
                    Just (B_not expr, restTokens)
                result -> result

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