module Interpreter where

import Inst (Inst (..), Code (..))
import Stack
import Storage
import Value

data InterpreterState = 
    InterpreterState {
        code :: Code,
        stack :: Stack,
        storage :: Storage
    }

initialState :: Code -> InterpreterState
initialState code = InterpreterState code createEmptyStack createEmptyStorage
                
exec :: InterpreterState -> InterpreterState
exec (InterpreterState [] stack store) = InterpreterState [] stack store
exec (InterpreterState (inst:xs) stack store) =
    case inst of
        Push value ->
            let newStack = Stack.push stack (Value.Integer value)
            in InterpreterState xs newStack store
        -- Handle other instructions similarly                
        
-- Push
-- Add
-- Mul
-- Sub
-- PushTrue
-- PushFalse
-- Eq
-- Le
-- And
-- Neg
-- Fetch
-- Store
-- Noop
-- Branch
-- Loop