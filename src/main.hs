import Compiler
import Parser
import Interpreter
import Stack
import State
import Inst

testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

main :: IO ()
main = do 
    putStrLn $ "Assembler test 1: " ++ show (testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10",""))
    putStrLn $ "Assembler test 2: " ++ show (testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True"))
    putStrLn $ "Assembler test 3: " ++ show (testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False"))
    putStrLn $ "Assembler test 4: " ++ show (testAssembler [Push (-20),Tru,Fals] == ("False,True,-20",""))
    putStrLn $ "Assembler test 5: " ++ show (testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20",""))
    putStrLn $ "Assembler test 6: " ++ show (testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20",""))
    putStrLn $ "Assembler test 7: " ++ show (testAssembler [Push (-20),Push (-21), Le] == ("True",""))
    putStrLn $ "Assembler test 8: " ++ show (testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4"))
    putStrLn $ "Assembler test 9: " ++ show (testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1"))
    putStrLn $ "Parser test 1: " ++ show (testParser "x := 5; x := x - 1;" == ("","x=4"))
    putStrLn $ "Parser test 2: " ++ show (testParser "x := 0 - 2;" == ("","x=-2"))
    putStrLn $ "Parser test 3: " ++ show (testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2"))
    putStrLn $ "Parser test 4: " ++ show (testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1"))
    putStrLn $ "Parser test 5: " ++ show (testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2"))
    putStrLn $ "Parser test 6: " ++ show (testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4"))
    putStrLn $ "Parser test 7: " ++ show (testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68"))
    putStrLn $ "Parser test 8: " ++ show (testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34"))
    putStrLn $ "Parser test 9: " ++ show (testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1"))
    putStrLn $ "Parser test 10: " ++ show (testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2"))
    putStrLn $ "Parser test 11: " ++ show (testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6"))
    putStrLn $ "Parser test 12: " ++ show (testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1"))
    -- putStrLn $ "Assembler error 1: " ++ show (testAssembler [Push 1,Push 2,And]) -- Should get Run-time error
    -- putStrLn $ "Assembler error 2: " ++ show (testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]) -- Should get Run-time error
