module Stack where

import Data.List (intercalate)
import Value

-- Definition of new type for a stack of values.
newtype Stack = Stack [Val]

-- Function to create an empty stack.
createEmptyStack :: Stack
createEmptyStack = Stack []

-- Function responsible for pushing a value on to the stack.
push :: Stack -> Val -> Stack
push (Stack stack) value = Stack (value : stack)

-- Function responsible for popping a value from the stack.
pop :: Stack -> Stack
pop (Stack (_:xs)) = Stack xs
pop _ = error "Run-time error"

-- Function responsible for returning the value on top of the stack.
top :: Stack -> Val
top (Stack (x:_)) = x
top _ = error "Run-time error"

-- Function responsible for converting the stack to a string.
stack2Str :: Stack -> String
stack2Str (Stack xs) = intercalate "," (map value2Str xs)