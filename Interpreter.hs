module Interpreter where

import Inst (Inst (..), Code (..))
import Stack
import State
import Value

exec :: (Code, Stack, State) -> Maybe (Code, Stack, State)
exec ([], stack, state) = Nothing
exec (inst:xs, stack, state) =
    case inst of
        Push val -> 
            let newStack = Stack.push stack (Integer val)
            in Just (xs, newStack, state)
        Add ->
            case (top stack, top (pop stack)) of
                (Integer a, Integer b) ->
                    let newStack = Stack.push (pop (pop stack)) (Integer (a + b))
                    in Just (xs, newStack, state)
                _ -> error "Run-time error"
        Mult ->
            case (top stack, top (pop stack)) of
                (Integer a, Integer b) ->
                    let newStack = Stack.push (pop (pop stack)) (Integer (a * b))
                    in Just (xs, newStack, state)
                _ -> error "Run-time error"
        Sub ->
            case (top stack, top (pop stack)) of
                (Integer a, Integer b) ->
                    let newStack = Stack.push(pop(pop stack)) (Integer (a - b))
                    in Just (xs,newStack, state)
                _ -> error "Run-time error"
        Tru -> 
            let newStack = Stack.push stack Tt
            in Just (xs, newStack, state)
        Fals ->
            let newStack = Stack.push stack Ff
            in Just (xs, newStack, state)
        Equ ->
            case (top stack, top (pop stack)) of
                (val1, val2) | val1 == val2 ->
                    let newStack = Stack.push (pop (pop stack)) Tt
                    in Just (xs, newStack, state)
                _ ->
                    let newStack = Stack.push (pop (pop stack)) Ff
                    in Just (xs, newStack, state)
        Le ->
            case (top stack, top (pop stack)) of
                (Integer a, Integer b) ->
                    let newStack = Stack.push (pop (pop stack)) (if a <= b then Tt else Ff)
                    in Just (xs, newStack, state)
                _ -> error "Run-time error"
        And -> 
            case (top stack, top (pop stack)) of
                (Tt, Tt) ->
                    let newStack = Stack.push (pop (pop stack)) Tt
                    in Just (xs, newStack, state)
                (Tt, Ff) ->
                    let newStack = Stack.push (pop (pop stack)) Ff
                    in Just (xs, newStack, state)
                (Ff, Tt) ->
                    let newStack = Stack.push (pop (pop stack)) Ff
                    in Just (xs, newStack, state)
                (Ff, Ff) ->
                    let newStack = Stack.push (pop (pop stack)) Ff
                    in Just (xs, newStack, state)
                _ -> error "Run-time error"
        Neg ->
            case top stack of
                Tt ->
                    let newStack = Stack.push (pop stack) Ff
                    in Just (xs, newStack, state)
                Ff ->
                    let newStack = Stack.push (pop stack) Tt
                    in Just (xs, newStack, state)
                _ -> error "Run-time error"
        Fetch var ->
            case get state var of
                Just val ->
                    let newStack = Stack.push stack val
                    in Just (xs, newStack, state)
                Nothing -> 
                    error "Run-time error"
        Store var ->
            let val = top stack
                newState = State.push state var val
            in Just (xs, pop stack, newState)
        Noop -> 
            Just (xs, stack, state)
        Branch code1 code2 ->
            case top stack of
                Tt -> Just (code1 ++ xs, pop stack, state)
                Ff -> Just (code2 ++ xs, pop stack, state)
                _ -> error "Run-time error"
        Loop code1 code2 ->
            let code = code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]] ++ xs
            in Just (code, stack, state)

run :: (Code, Stack, State) -> (Code, Stack, State)
run (code, stack, state) = 
    case exec (code, stack, state) of
        Just nextInterpreter -> run nextInterpreter
        Nothing -> (code, stack, state)