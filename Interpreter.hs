module Interpreter where

import Inst (Inst (..), Code (..))
import Stack
import State
import Value

exec :: (Code, Stack, State) -> Maybe (Code, Stack, State)
exec ([], stack, store) = Nothing
exec (inst:xs, stack, store) =
    case inst of
        Push value ->
            let newStack = Stack.push stack (Integer value)
            in Just (xs, newStack, store)
        Add ->
            case (top stack, top (pop stack)) of
                (Integer a, Integer b) ->
                    let newStack = Stack.push (pop (pop stack)) (Integer (a + b))
                    in Just (xs, newStack, store)
                _ -> error "Add: Attempt to add non-integer values"
        Mult ->
            case (top stack, top (pop stack)) of
                (Integer a, Integer b) ->
                    let newStack = Stack.push (pop (pop stack)) (Integer (a * b))
                    in Just (xs, newStack, store)
                _ -> error "Mul: Attempt to multiply non-integer values"
        Sub ->
            case (top stack, top (pop stack)) of
                (Integer a, Integer b) ->
                    let newStack = Stack.push(pop(pop stack)) (Integer (a - b))
                    in Just (xs,newStack, store )
                _ -> error "Sub: Attempt to subtract non-integer values"
        Tru -> 
            let newStack = Stack.push stack Tt
            in Just (xs, newStack, store)
        Fals ->
            let newStack = Stack.push stack Ff
            in Just (xs, newStack, store)
        Equ ->
            case (top stack, top (pop stack)) of
                (val1, val2) | val1 == val2 ->
                    let newStack = Stack.push (pop (pop stack)) Tt
                    in Just (xs, newStack, store)
                _ ->
                    let newStack = Stack.push (pop (pop stack)) Ff
                    in Just (xs, newStack, store)
        Le ->
            case (top stack, top (pop stack)) of
                (Integer a, Integer b) ->
                    let newStack = Stack.push (pop (pop stack)) (if a <= b then Tt else Ff)
                    in Just (xs, newStack, store)
                _ -> error "Le: Attempt to compare non-integer values"
        And -> 
            case (top stack, top (pop stack)) of
                (Tt, Tt) ->
                    let newStack = Stack.push (pop (pop stack)) Tt
                    in Just (xs, newStack, store)
                _ ->
                    let newStack = Stack.push (pop (pop stack)) Ff
                    in Just (xs, newStack, store)
        Neg ->
            case top stack of
                Tt ->
                    let newStack = Stack.push (pop stack) Ff
                    in Just (xs, newStack, store)
                Ff ->
                    let newStack = Stack.push (pop stack) Tt
                    in Just (xs, newStack, store)
                _ -> error "Neg: Attempt to negate non-boolean value"

run :: (Code, Stack, State) -> (Code, Stack, State)
run (code, stack, state) = 
    case exec (code, stack, state) of
        Just nextInterpreter -> run nextInterpreter
        Nothing -> (code, stack, state)

-- Push                 | ✓
-- Add                  | ✓ 
-- Mult                 | ✓
-- Sub                  | ✓
-- Tru                  | ✓ 
-- Fals                 | ✓
-- Equ                  | ✓
-- Le                   | ✓
-- And                  |
-- Neg                  |
-- Fetch                |
-- Store                |
-- Noop                 |
-- Branch               |
-- Loop                 |