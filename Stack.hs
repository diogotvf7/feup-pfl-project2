module Stack where

import Data.List (intercalate)
import Value

newtype Stack = Stack [Val]

createEmptyStack :: Stack
createEmptyStack = Stack []

push :: Stack -> Val -> Stack
push (Stack stack) value = Stack (value : stack)

pop :: Stack -> Stack
pop (Stack (_:xs)) = Stack xs
pop _ = error "Run-time error"

top :: Stack -> Val
top (Stack (x:_)) = x
top _ = error "Run-time error"

stack2Str :: Stack -> String
stack2Str (Stack xs) = intercalate "," (map value2Str xs)