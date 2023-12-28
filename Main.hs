module Main where

import Interpreter
import Inst
import Stack (stack2Str)
import Storage (storage2Str)
-- import Value

-- Example code with a Push instruction
exampleCode1 :: Code
exampleCode1 = [Push 42, Push 10, Push 5]

-- Example code with an Add instruction
exampleCode2 :: Code
exampleCode2 = [Push 42, Push 10, Add]

-- Example code with an Add instruction (Attempt to read on null stack)
exampleCode3 :: Code
exampleCode3 = [Push 10, Add]

-- Example code with an Add instruction (Attempt to add non-integer values)
-- exampleCode4 :: Code
-- exampleCode4 = [Push 10, Push Tt, Add]

-- Main function
main :: IO ()
main = do
    -- Create the initial state
    let initialState' = initialState exampleCode1

    -- Execute the code using the exec function
    let finalState = run initialState'
    
    -- Print the final state information
    putStrLn "Final Code:"
    print $ code finalState
    
    putStrLn "Final Stack:"
    putStrLn $ stack2Str $ stack finalState
    
    putStrLn "Final Storage:"
    putStrLn $ storage2Str $ storage finalState