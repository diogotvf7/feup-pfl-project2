module Main where

import Interpreter
import Inst
import Stack (Stack, createEmptyStack, stack2Str)
import State (State, createEmptyState, state2Str)

-- Part1 tests
main :: IO ()
main = do
  putStrLn $ "Test 1: " ++ show (testAssembler [Push 42, Push 10, Push 5] == ("5,10,42", ""))
  putStrLn $ "Test 2: " ++ show (testAssembler [Push 42, Push 10, Add] == ("52", ""))
  putStrLn $ "Test 3: " ++ show (testAssembler [Push 42, Push 10, Mult] == ("420", ""))
  putStrLn $ "Test 4: " ++ show (testAssembler [Push 10, Push 42, Sub] == ("32", ""))
  putStrLn $ "Test 5: " ++ show (testAssembler [Tru, Tru, Tru] == ("True,True,True", ""))
  putStrLn $ "Test 6: " ++ show (testAssembler [Fals, Fals, Fals] == ("False,False,False", ""))
  putStrLn $ "Test 7: " ++ show (testAssembler [Push 42, Push 42, Equ] == ("True", ""))
  putStrLn $ "Test 8: " ++ show (testAssembler [Push 42, Push 10, Equ] == ("False", ""))
  putStrLn $ "Test 9: " ++ show (testAssembler [Tru, Fals, Equ] == ("False", ""))
  putStrLn $ "Test 11: " ++ show (testAssembler [Tru, Tru, Equ] == ("True", ""))
  putStrLn $ "Test 12: " ++ show (testAssembler [Push 42, Push 42, Le] == ("True", ""))
  putStrLn $ "Test 13: " ++ show (testAssembler [Push 42, Push 10, Le] == ("True", ""))
  putStrLn $ "Test 14: " ++ show (testAssembler [Push 10, Push 42, Le] == ("False", ""))
  putStrLn $ "Test 15: " ++ show (testAssembler [Tru, Tru, And] == ("True", ""))
  putStrLn $ "Test 16: " ++ show (testAssembler [Tru, Fals, And] == ("False", ""))
  putStrLn $ "Test 17: " ++ show (testAssembler [Fals, Tru, And] == ("False", ""))
  putStrLn $ "Test 18: " ++ show (testAssembler [Fals, Fals, And] == ("False", ""))
  putStrLn $ "Test 19: " ++ show (testAssembler [Tru, Neg] == ("False", ""))
  putStrLn $ "Test 20: " ++ show (testAssembler [Fals, Neg] == ("True", ""))
  putStrLn $ "Test 21: " ++ show (testAssembler [Push 42, Store "var", Fetch "var"] == ("42", "var=42"))
  putStrLn $ "Test 22: " ++ show (testAssembler [Tru, Store "var", Fetch "var"] == ("True", "var=True"))
  putStrLn $ "Test 23: " ++ show (testAssembler [Noop] == ("", ""))
  putStrLn $ "Test 24: " ++ show (testAssembler [Push 42, Noop] == ("42", ""))
  putStrLn $ "Test 25: " ++ show (testAssembler [Push 42, Noop, Push 10, Noop, Noop] == ("10,42", ""))
  putStrLn $ "Test 26: " ++ show (testAssembler [Tru, Branch [Push 42] [Push 10]] == ("42", ""))
  putStrLn $ "Test 27: " ++ show (testAssembler [Fals, Branch [Push 42] [Push 10]] == ("10", ""))
  putStrLn $ "Test 28: " ++ show (testAssembler [Push 42, Tru, Branch [Push 42] [Push 10]] == ("42,42", ""))
  putStrLn $ "Test 29: " ++ show (testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1"))
  putStrLn $ "Test 26: " ++ show (testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10",""))
  putStrLn $ "Test 27: " ++ show (testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True"))
  putStrLn $ "Test 28: " ++ show (testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False"))
  putStrLn $ "Test 29: " ++ show (testAssembler [Push (-20),Tru,Fals] == ("False,True,-20",""))
  putStrLn $ "Test 30: " ++ show (testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20",""))
  putStrLn $ "Test 31: " ++ show (testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20",""))
  putStrLn $ "Test 32: " ++ show (testAssembler [Push (-20),Push (-21), Le] == ("True",""))
  putStrLn $ "Test 33: " ++ show (testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4"))
  putStrLn $ "Test 34: " ++ show (testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1"))
  
  -- -- Test error due to attempt to read on null stack
  -- testAssembler [Push 10, Add]

  -- -- Test error due to attempt to add non-integer values
  -- testAssembler [Push 42, Push 10, Mul]

  -- -- Test error due to attempt to compare non-integer values
  -- testAssembler [Tru, Tru, Le]

  -- -- Test error due to attempt to perform a conjunction with Integer values
  -- testAssembler [Push 1,Push 2,And]

  -- -- Test error due to trying to fetch avariable that isn't assigned
  -- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]


testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)
