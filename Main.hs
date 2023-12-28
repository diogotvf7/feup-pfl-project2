-- ----------------------------------------------------
-- Main.hs
module Main where

import Interpreter
import Inst

-- Example code with a Push instruction
exampleCode :: Code
exampleCode = [Push 42, Push 10, Push 5]

main :: IO ()
main = do
    -- Create the initial state
    let initialState = initialState exampleCode
    
    -- Execute the code using the exec function
    let finalState = exec initialState
    
    -- Print the final state information
    putStrLn "Final Code:"
    print $ code finalState
    
    putStrLn "Final Stack:"
    putStrLn $ stack2Str $ stack finalState
    
    putStrLn "Final Storage:"
    putStrLn $ storage2Str $ storage finalState
    


