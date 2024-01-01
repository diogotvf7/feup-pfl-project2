module Parser where 

import Lexer

-- Data type for arithmetic expressions.
data Aexp
    = A_var String           -- Variable
    | A_num Integer          -- Integer constant
    | A_add Aexp Aexp        -- Addition
    | A_sub Aexp Aexp        -- Subtraction
    | A_mul Aexp Aexp        -- Multiplication
    deriving (Eq, Show)

-- Data type for boolean expressions.
data Bexp
    = B_true                 -- True
    | B_false                -- False
    | B_aeq Aexp Aexp        -- Arithmetic equality
    | B_beq Bexp Bexp        -- Boolean equality
    | B_leq Aexp Aexp        -- Less than or equal to
    | B_not Bexp             -- Logical NOT
    | B_and Bexp Bexp        -- Logical AND
    deriving (Eq, Show)

-- Data type for statements.
data Stm
    = S_assign String Aexp   -- Assignment statement
    | S_if Bexp [Stm] [Stm]  -- If statement with true and false branches
    | S_while Bexp [Stm]     -- While loop
    deriving (Eq, Show)

-- Alias definition for a program, which is a list of statements.
type Program = [Stm]

-- Function to parse a program from a string input.
parse :: String -> Program
parse input = parseStatements (lexer input)

-- Parse a list of tokens into a list of statements.
parseStatements :: [Token] -> [Stm]
parseStatements [] = []
parseStatements (T_var var : T_assign : tokens) =
    let 
        aexp = parseA (takeWhile (/= T_semicolon) tokens)  -- Parse arithmetic expression.
    in
        S_assign var aexp : parseStatements (drop 1 (dropWhile (/= T_semicolon) tokens))

-- Parsing of a while loop
parseStatements (T_while : tokens) = 
    let 
        bexp = parseB (takeWhile (/= T_do) tokens)                            -- Parse boolean expression.
        (body, restTokens) = parseBody (drop 1 (dropWhile (/= T_do) tokens))  -- Parse while loop body.
    in
        S_while bexp (parseStatements body) : parseStatements (drop 1 (dropWhile (/= T_semicolon) restTokens))

-- Parsing of a if->then / else
parseStatements (T_if : tokens) =
    let 
        bexp = parseB (takeWhile (/= T_then) tokens)                                        -- Parse boolean expression.
        (trueBranch, restTokens1) = parseBody (drop 1 (dropWhile (/= T_then) tokens))       -- Parse true branch of if statement.
        (falseBranch, restTokens2) = parseBody (drop 1 (dropWhile (/= T_else) restTokens1)) -- Parse false branch of if statement.
    in
        S_if bexp (parseStatements trueBranch) (parseStatements falseBranch) : parseStatements restTokens2

parseStatements (T_semicolon : tokens) = parseStatements tokens

-- Parse the body of a block, enclosed in parentheses.
parseBody :: [Token] -> ([Token], [Token])
parseBody (T_lbracket : tokens) = parseBodyAux tokens [] [T_lbracket]
parseBody tokens = (takeWhile (\t -> t /= T_semicolon && t /= T_then && t /= T_else) tokens,
                   drop 1 (dropWhile (\t -> t /= T_semicolon && t /= T_then && t /= T_else) tokens))

-- Auxiliary function to help parse the body of a block
parseBodyAux :: [Token] -> [Token] -> [Token] -> ([Token], [Token])
parseBodyAux [] body stack = error "Run-time error"                                        -- Unmatched brackets.
parseBodyAux (T_lbracket : tokens) body stack = parseBodyAux tokens (body ++ [T_lbracket]) (T_lbracket : stack)
parseBodyAux (T_rbracket : tokens) body (T_lbracket : stack) = 
    if stack == [] then (body, tokens)                                                     -- Successfully matched brackets.
    else parseBodyAux tokens (body ++ [T_rbracket]) stack
parseBodyAux (token : tokens) body stack = parseBodyAux tokens (body ++ [token]) stack


-- --------------------------------- PARSE A -------------------------------------------

-- Function to parse an arithmetic expression (Aexp) from a list of tokens.
parseA :: [Token] -> Aexp
parseA tokens =
    case parseSumOrSubOrProdOrIntOrVarOrPar tokens of
        Just (expr, []) -> expr
        _ -> error "Run-time error"

-- Parsing integer constants, variables, or expressions within parentheses.
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

-- Parsing multiplication, integer constants, variables, or expressions within parentheses.
parseProdOrIntOrVarOrPar :: [Token] -> Maybe (Aexp, [Token])
parseProdOrIntOrVarOrPar tokens
    = case parseIntOrVarOrPar tokens of
        Just (expr1, (T_times : restTokens1)) ->
            case parseProdOrIntOrVarOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (A_mul expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result

-- Parsing addition, subtraction, multiplication, integer constants, variables, or expressions within parentheses.
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

-- --------------------------------- PARSE B -------------------------------------------

-- Function to parse a boolean expression (Bexp) from a list of tokens.
parseB :: [Token] -> Bexp
parseB tokens =
    case parseAndOrBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar tokens of
        Just (expr, []) -> expr
        _ -> error "Run-time error"

-- Parsing True, False or expressions withing parentheses.
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

-- Parsing less than or equal (Leq) expressions, True, False, or expressions within parentheses.
parseLeqOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseLeqOrTrueOrFalseOrPar tokens
    = case parseSumOrSubOrProdOrIntOrVarOrPar tokens of
        Just (expr1, (T_leq : restTokens1)) ->
            case parseSumOrSubOrProdOrIntOrVarOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (B_leq expr1 expr2, restTokens2)
                Nothing -> Nothing
        _ -> parseTrueOrFalseOrPar tokens

-- Parsing arithmetic equality (Aeq), less than or equal (Leq), True, False, or expressions within parentheses.
parseAeqOrLeqOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseAeqOrLeqOrTrueOrFalseOrPar tokens
    = case parseSumOrSubOrProdOrIntOrVarOrPar tokens of
        Just (expr1, (T_aeq : restTokens1)) ->
            case parseSumOrSubOrProdOrIntOrVarOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (B_aeq expr1 expr2, restTokens2)
                Nothing -> Nothing
        _ -> parseLeqOrTrueOrFalseOrPar tokens

-- Parsing boolean negation (Not), arithmetic equality (Aeq), less than or equal (Leq), True, False, or expressions within parentheses.
parseNotOrAeqOrLeqOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseNotOrAeqOrLeqOrTrueOrFalseOrPar (T_not : restTokens)
    = case parseAeqOrLeqOrTrueOrFalseOrPar restTokens of
        Just (expr1, restTokens1) ->
            Just (B_not expr1, restTokens1)
        result -> parseAeqOrLeqOrTrueOrFalseOrPar restTokens
parseNotOrAeqOrLeqOrTrueOrFalseOrPar tokens = parseAeqOrLeqOrTrueOrFalseOrPar tokens

-- Parsing boolean equality (Beq), boolean negation (Not), arithmetic equality (Aeq),
-- less than or equal (Leq), True, False, or expressions within parentheses.
parseBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar tokens
    = case parseNotOrAeqOrLeqOrTrueOrFalseOrPar tokens of
        Just (expr1, (T_beq : restTokens1)) ->
            case parseBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (B_beq expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result

-- Parsing logical AND (And), boolean equality (Beq), boolean negation (Not), arithmetic equality (Aeq),
-- less than or equal (Leq), True, False, or expressions within parentheses.
parseAndOrBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseAndOrBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar tokens
    = case parseBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar tokens of 
        Just (expr1, (T_and : restTokens1)) ->
            case parseAndOrBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (B_and expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result