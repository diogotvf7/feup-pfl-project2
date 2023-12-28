module Interpreter where

import Inst (Inst (..), Code (..))
import Stack
import Storage
import Value

data State = 
    State {
        code :: Code,
        stack :: Stack,
        storage :: Storage
    }

initialState :: Code -> State
initialState code = State code createEmptyStack createEmptyStorage
                
exec :: State -> Maybe State
exec (State [] stack store) = Nothing
exec (State (inst:xs) stack store) =
    case inst of
        Push value ->
            let newStack = Stack.push stack (Value.Integer value)
            in Just $ State xs newStack store
        Add ->
            case (top stack, top (pop stack)) of
                (Integer a, Integer b) ->
                    let newStack = Stack.push (pop (pop stack)) (Value.Integer (a + b))
                    in Just $ State xs newStack store
                _ -> error "Add: Attempt to add non-integer values"
          
run :: State -> State
run state =
    case exec state of
        Just nextState -> run nextState
        Nothing -> state

-- Push                 | ✓
-- Add                  | 
-- Mul                  |
-- Sub                  |
-- PushTrue             |
-- PushFalse            |
-- Eq                   |
-- Le                   |
-- And                  |
-- Neg                  |
-- Fetch                |
-- Store                |
-- Noop                 |
-- Branch               |
-- Loop                 |