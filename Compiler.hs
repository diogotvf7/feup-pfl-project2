module Compiler where

import Parser
import Inst

compA :: Aexp -> Code
compA (A_var varName) = [Fetch varName]
compA (A_num num) = [Push num]
compA (A_add aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Add]
compA (A_sub aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Sub]
compA (A_mul aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Mult]

compB :: Bexp -> Code
compB (B_true) = [Tru]
compB (B_false) = [Fals]
compB (B_aeq aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Equ]
compB (B_beq bexp1 bexp2) = compB bexp2 ++ compB bexp1 ++ [Equ]
compB (B_leq aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Le]
compB (B_not bexp1) = compB bexp1 ++ [Neg]
compB (B_and bexp1 bexp2) = compB bexp1 ++ compB bexp2 ++ [And]

compile :: Program -> Code
compile [] = []
compile (S_assign var aexp : prog) = compA aexp ++ [Store var] ++ compile prog
compile (S_if bexp stm1 stm2 : prog) = 
    let
        on_true = compile stm1
        on_false = compile stm2
        condition = compB bexp
    in
        condition ++ [Branch on_true on_false] ++ compile prog
compile (S_while bexp stm : prog) = 
    let
        on_true = compile stm
        condition = compB bexp
    in
        [Loop on_true condition] ++ compile prog
        
-- compile (stm:stms) =
--     case stm of
--         S_assign var aexp ->
--             compA aexp ++ [Store var] ++ compile stms
--         S_if bexp stm1 stm2 ->
--             let
--                 on_true = compile stm1
--                 on_false = compile stm2
--                 condition = compB bexp
--             in
--                 condition ++ [Branch on_true on_false] ++ compile stms
--         S_while bexp stm1 ->
--             let
--                 on_true = compile stm1
--                 condition = compB bexp
--             in
--                 [Loop on_true condition] ++ compile stms