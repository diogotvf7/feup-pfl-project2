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
parseStatements tokens =
    let
        (stm, tokens') = parseStatement tokens
    in
        stm : parseStatements tokens'
    
parseStatement :: [Token] -> (Stm, [Token])
parseStatement (T_var varName : T_assign : tokens) =
    let
        (aexp, tokens') = parseAexp tokens
    in
        (S_assign varName aexp, tokens')
parseStatement (T_if : tokens) =
    let
        (bexp, tokens') = parseBexp tokens
        (stms1, tokens'') = parseStatements tokens'
        (stms2, tokens''') = parseStatements tokens''
    in
        (S_if bexp stms1 stms2, tokens''')
parseStatement (T_while : tokens) =
    let
        (bexp, tokens') = parseBexp tokens
        (stms, tokens'') = parseStatements tokens'
    in
        (S_while bexp stms, tokens'')
parseStatement tokens = error ("parseStatement: unexpected tokens " ++ show tokens)

parseAexp :: [Token] -> (Aexp, [Token])
parseAexp tokens =
    let
        (aexp, tokens') = parseAexp1 tokens
    in
        parseAexp' aexp tokens'

parseAexp' :: Aexp -> [Token] -> (Aexp, [Token])
parseAexp' aexp (T_plus : tokens) =
    let
        (aexp', tokens') = parseAexp1 tokens
    in
        parseAexp' (A_add aexp aexp') tokens'
parseAexp' aexp (T_less : tokens) =
    let
        (aexp', tokens') = parseAexp1 tokens
    in
        parseAexp' (A_sub aexp aexp') tokens'
parseAexp' aexp tokens = (aexp, tokens)

parseAexp1 :: [Token] -> (Aexp, [Token])
parseAexp1 tokens =
    let
        (aexp, tokens') = parseAexp2 tokens
    in
        parseAexp1' aexp tokens'

parseAexp1' :: Aexp -> [Token] -> (Aexp, [Token])
parseAexp1' aexp (T_times : tokens) =
    let
        (aexp', tokens') = parseAexp2 tokens
    in
        parseAexp1' (A_mul aexp aexp') tokens'
parseAexp1' aexp tokens = (aexp, tokens)

parseAexp2 :: [Token] -> (Aexp, [Token])
parseAexp2 (T_var varName : tokens) = (A_var varName, tokens)
parseAexp2 (T_integer num : tokens) = (A_num num, tokens)
parseAexp2 (T_lbracket : tokens) =
    let
        (aexp, tokens') = parseAexp tokens
    in
        case tokens' of
            T_rbracket : tokens'' -> (aexp, tokens'')
            _ -> error ("parseAexp2: expected ) but got " ++ show tokens')
parseAexp2 tokens = error ("parseAexp2: unexpected tokens " ++ show tokens)

parseBexp :: [Token] -> (Bexp, [Token])
parseBexp tokens =
    let
        (bexp, tokens') = parseBexp1 tokens
    in
        parseBexp' bexp tokens'

parseBexp' :: Bexp -> [Token] -> (Bexp, [Token])
parseBexp' bexp (T_beq : tokens) =
    let
        (bexp', tokens') = parseBexp1 tokens
    in
        parseBexp' (B_beq bexp bexp') tokens'
parseBexp' bexp (T_lesseq : tokens) =
    let
        (bexp', tokens') = parseBexp1 tokens
    in
        parseBexp' (B_leq bexp bexp') tokens'
parseBexp' bexp tokens = (bexp, tokens)

parseBexp1 :: [Token] -> (Bexp, [Token])
parseBexp1 (T_bool True : tokens) = (B_true, tokens)
parseBexp1 (T_bool False : tokens) = (B_false, tokens)
parseBexp1 (T_lbracket : tokens) =
    let
        (bexp, tokens') = parseBexp tokens
    in
        case tokens' of
            T_rbracket : tokens'' -> (bexp, tokens'')
            _ -> error ("parseBexp1: expected ) but got " ++ show tokens')
parseBexp1 (T_not : tokens) =
    let
        (bexp, tokens') = parseBexp1 tokens
    in
        (B_not bexp, tokens')
parseBexp1 tokens =
    let
        (aexp1, tokens') = parseAexp tokens
    in
        parseBexp1' aexp1 tokens'

parseBexp1' :: Aexp -> [Token] -> (Bexp, [Token])
parseBexp1' aexp1 (T_aeq : tokens) =
    let
        (aexp2, tokens') = parseAexp tokens
    in
        (B_aeq aexp1 aexp2, tokens')
parseBexp1' aexp1 (T_and : tokens) =
    let
        (aexp2, tokens') = parseAexp tokens
    in
        (B_and (B_aeq aexp1 aexp2) (B_aeq aexp2 aexp1), tokens')
parseBexp1' aexp1 tokens = (B_aeq aexp1 (A_num 0), tokens)

    
