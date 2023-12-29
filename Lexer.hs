module Lexer where

import Data.Char (isSpace, isDigit, isAlpha)

data Token
    = T_while                 -- | while
    | T_if                    -- | if
    | T_then                  -- | then
    | T_else                  -- | else
    | T_not                   -- | not
    | T_plus                  -- | +
    | T_less                  -- | -
    | T_times                 -- | *
    | T_semicolon             -- | ;
    | T_lbracket              -- | (
    | T_rbracket              -- | )
    | T_assign                -- | :=
    | T_eq                    -- | =
    | T_eqeq                  -- | ==
    | T_and                   -- | and
    | T_lesseq                -- | <=
    | T_integer Integer       -- | Number
    | T_bool Bool             -- | Boolean
    | T_var String            -- | Variable
    deriving (Eq, Show)

lexer :: String -> [Token]
lexer [] = []
lexer ('w':'h':'i':'l':'e':restStr) = T_while : lexer restStr
lexer ('i':'f':restStr) = T_if : lexer restStr
lexer ('t':'h':'e':'n':restStr) = T_then : lexer restStr
lexer ('e':'l':'s':'e':restStr) = T_else : lexer restStr
lexer ('n':'o':'t':restStr) = T_not : lexer restStr
lexer ('+':restStr) = T_plus : lexer restStr
lexer ('-':restStr) = T_less : lexer restStr
lexer ('*':restStr) = T_times : lexer restStr
lexer (';':restStr) = T_semicolon : lexer restStr
lexer ('(':restStr) = T_lbracket : lexer restStr
lexer (')':restStr) = T_rbracket : lexer restStr
lexer (':':'=':restStr) = T_assign : lexer restStr
lexer ('<':'=':restStr) = T_lesseq : lexer restStr
lexer ('=':'=':restStr) = T_eqeq : lexer restStr
lexer ('=':restStr) = T_eq : lexer restStr
lexer ('a':'n':'d':restStr) = T_and : lexer restStr
lexer ('T':'r':'u':'e':restStr) = T_bool True : lexer restStr
lexer ('F':'a':'l':'s':'e':restStr) = T_bool False : lexer restStr
lexer (chr:restStr)
    | isSpace chr = lexer restStr
    | isDigit chr = lexNum (chr:restStr)
    | isAlpha chr = lexVar (chr:restStr)
    | otherwise = error ("lexer: unexpected character " ++ [chr])

lexNum :: String -> [Token]
lexNum str = T_integer (read numStr) : lexer restStr
    where (numStr, restStr) = span isDigit str

lexVar :: String -> [Token]
lexVar str = T_var varStr : lexer restStr
    where (varStr, restStr) = span isAlpha str

