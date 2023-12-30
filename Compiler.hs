module Compiler where

compA :: Aexp -> Code
compA (A_var varName) = [Fetch varName]
compA (A_num num) = [Push num]
compA (A_add aexp1 aexp2) = [compA aexp2, compA aexp1, Add]
compA (A_sub aexp1 aexp2) = [compA aexp2, compA aexp1, Sub]
compA (A_mul aexp1 aexp2) = [compA aexp2, compA aexp1, Mul]

compB :: Bexp -> Code
compB (B_true) = [Tru]
compB (B_false) = [Fals]
compB (B_aeq aexp1 aexp2) = [compA aexp2, compA aexp1, Equ]
compB (B_beq bexp1 bexp2) = [compB bexp2, compB bexp1, Equ]
compB (B_leq aexp1 aexp2) = [compA aexp2, compA aexp1, Le]
compB (B_not bexp1) = [compB bexp1, Neg]
compB (B_and bexp1 bexp2) = [compB bexp1, compB bexp2, And]

compile :: Program -> Code
compile [] = []
compile statements = 
    case stms of 
        S_assign : xs ->
            let 



                            -- | ✓ 
S_assign String Aexp        -- |  
S_if Bexp [Stm] [Stm]       -- | 
S_while Bexp [Stm]          -- | 