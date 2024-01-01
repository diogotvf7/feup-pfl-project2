module Compiler where

import Parser
import Inst

-- Function to compile arithmetic expressions (Aexp) to instructions (Code).
compA :: Aexp -> Code
compA (A_var varName) = [Fetch varName]                             -- Fetch value from a variable
compA (A_num num) = [Push num]                                      -- Push an integer on to the stack
compA (A_add aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Add]     -- Addition
compA (A_sub aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Sub]     -- Subtraction
compA (A_mul aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Mult]    -- Multiplication

-- Function to compile boolean expression (Bexp) to instructions (Code).
compB :: Bexp -> Code
compB (B_true) = [Tru]                                              -- Push True on to the stack
compB (B_false) = [Fals]                                            -- Push False on to the stack
compB (B_aeq aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Equ]     -- Arithmetic equality
compB (B_beq bexp1 bexp2) = compB bexp2 ++ compB bexp1 ++ [Equ]     -- Boolean equality
compB (B_leq aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Le]      -- Less than or equal
compB (B_not bexp1) = compB bexp1 ++ [Neg]                          -- Boolean negation
compB (B_and bexp1 bexp2) = compB bexp1 ++ compB bexp2 ++ [And]     -- Boolean AND

-- Function to compile a program to instructions (Code).
compile :: Program -> Code
compile [] = []                                                                 -- Empty program returns empty list of instructions
compile (S_assign var aexp : prog) = compA aexp ++ [Store var] ++ compile prog  -- Compile arithmetic expression
compile (S_if bexp stm1 stm2 : prog) =                                          -- Compile Branch with condtional execution
    let
        on_true = compile stm1
        on_false = compile stm2
        condition = compB bexp
    in
        condition ++ [Branch on_true on_false] ++ compile prog
compile (S_while bexp stm : prog) =                                             -- Compile Loop with condition execution 
    let
        on_true = compile stm
        condition = compB bexp
    in
        [Loop condition on_true] ++ compile prog