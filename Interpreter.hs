module Interpreter where

import Inst (Inst (..), Code (..))
import Stack
import State
import Value

-- Function to execute a step.
exec :: (Code, Stack, State) -> Maybe (Code, Stack, State)
exec ([], stack, state) = Nothing
exec (inst:xs, stack, state) =
    case inst of
        -- Code responsible for push a value on to the stack.
        Push val -> 
            let newStack = Stack.push stack (Integer val)
            in Just (xs, newStack, state)
        -- Code responsible for taking two values from the stack and adding them.
        Add ->
            case (top stack, top (pop stack)) of
                (Integer a, Integer b) ->
                    let newStack = Stack.push (pop (pop stack)) (Integer (a + b))
                    in Just (xs, newStack, state)
                _ -> error "Run-time error"
        -- Code responsible for taking two values from the stack and multiplying them.
        Mult ->
            case (top stack, top (pop stack)) of
                (Integer a, Integer b) ->
                    let newStack = Stack.push (pop (pop stack)) (Integer (a * b))
                    in Just (xs, newStack, state)
                _ -> error "Run-time error"
        -- Code responsible for taking two values from the stakc and subtracting them.
        Sub ->
            case (top stack, top (pop stack)) of
                (Integer a, Integer b) ->
                    let newStack = Stack.push(pop(pop stack)) (Integer (a - b))
                    in Just (xs,newStack, state)
                _ -> error "Run-time error"
        -- Code responsible for pushing True on to the stack.
        Tru -> 
            let newStack = Stack.push stack Tt
            in Just (xs, newStack, state)
        -- Code responsible for pushing False on to the stack.
        Fals ->
            let newStack = Stack.push stack Ff
            in Just (xs, newStack, state)
        -- Code responsible for checking if the top two values on the stack are equal. If so push True to the stack, otherwise push False.
        Equ ->
            case (top stack, top (pop stack)) of
                (val1, val2) | val1 == val2 ->
                    let newStack = Stack.push (pop (pop stack)) Tt
                    in Just (xs, newStack, state)
                _ ->
                    let newStack = Stack.push (pop (pop stack)) Ff
                    in Just (xs, newStack, state)
        -- Code responsible for checking if the top value on the stack is lesser or equals than the one under it. If it is push True to the stack otherwise push False.
        Le ->
            case (top stack, top (pop stack)) of
                (Integer a, Integer b) ->
                    let newStack = Stack.push (pop (pop stack)) (if a <= b then Tt else Ff)
                    in Just (xs, newStack, state)
                _ -> error "Run-time error"
        -- Code responsible for taking the top two values from the stack and applying a logical AND to them. Pushes the result to the stack.
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
        -- Code responsible for applying the logical NOT to the top value from the stack, pushes the result on to the stack.
        Neg ->
            case top stack of
                Tt ->
                    let newStack = Stack.push (pop stack) Ff
                    in Just (xs, newStack, state)
                Ff ->
                    let newStack = Stack.push (pop stack) Tt
                    in Just (xs, newStack, state)
                _ -> error "Run-time error"
        -- Code responsible for retrieving the value associated with a variable and pushes that value on to the stack. 
        Fetch var ->
            case get state var of
                Just val ->
                    let newStack = Stack.push stack val
                    in Just (xs, newStack, state)
                Nothing -> 
                    error "Run-time error"
        -- Code responsible for saving the value on top of the stack to a variable. Said value is popped from the stack.
        Store var ->
            let val = top stack
                newState = State.push state var val
            in Just (xs, pop stack, newState)
        -- Dummy function that returns the stack and state without any change.
        Noop -> 
            Just (xs, stack, state)
        -- Branch function takes the top value form the stack, if it is True it runs code1 if it is False, runs code2 instead.
        Branch code1 code2 ->
            case top stack of
                Tt -> Just (code1 ++ xs, pop stack, state)
                Ff -> Just (code2 ++ xs, pop stack, state)
                _ -> error "Run-time error"
        -- Loop function executed code1 while the value on top of the stack is True, when it is False executes code2 once and exits.
        Loop code1 code2 ->
            let code = code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]] ++ xs
            in Just (code, stack, state)


-- Function to run the interpreter.
run :: (Code, Stack, State) -> (Code, Stack, State)
run (code, stack, state) = 
    case exec (code, stack, state) of
        Just nextInterpreter -> run nextInterpreter
        Nothing -> (code, stack, state)