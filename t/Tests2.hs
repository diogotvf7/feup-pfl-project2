import Lexer

main :: IO ()
main = do
    putStrLn $ "Test T_while: " ++ show (testLexer "while" == [T_while])
    putStrLn $ "Test T_if: " ++ show (testLexer "if" == [T_if])
    putStrLn $ "Test T_then: " ++ show (testLexer "then" == [T_then])
    putStrLn $ "Test T_else: " ++ show (testLexer "else" == [T_else])
    putStrLn $ "Test T_not: " ++ show (testLexer "not" == [T_not])
    putStrLn $ "Test T_plus: " ++ show (testLexer "+" == [T_plus])
    putStrLn $ "Test T_less: " ++ show (testLexer "-" == [T_less])
    putStrLn $ "Test T_times: " ++ show (testLexer "*" == [T_times])
    putStrLn $ "Test T_semicolon: " ++ show (testLexer ";" == [T_semicolon])
    putStrLn $ "Test T_lbracket: " ++ show (testLexer "(" == [T_lbracket])
    putStrLn $ "Test T_rbracket: " ++ show (testLexer ")" == [T_rbracket])
    putStrLn $ "Test T_assign: " ++ show (testLexer ":=" == [T_assign])
    putStrLn $ "Test T_eq: " ++ show (testLexer "=" == [T_beq])
    putStrLn $ "Test T_eq: " ++ show (testLexer "==" == [T_aeq])
    putStrLn $ "Test T_and: " ++ show (testLexer "and" == [T_and])
    putStrLn $ "Test T_leq: " ++ show (testLexer "<=" == [T_leq])
    putStrLn $ "Test T_integer: " ++ show (testLexer "42" == [T_integer 42])
    putStrLn $ "Test T_bool: " ++ show (testLexer "True" == [T_bool True])
    putStrLn $ "Test T_bool: " ++ show (testLexer "False" == [T_bool False])
    putStrLn $ "Test T_var: " ++ show (testLexer "x" == [T_var "x"])
    putStrLn $ "Test 1: " ++ show (lexer "x := 5; x := x - 1;")
    putStrLn $ "Test 2: " ++ show (lexer "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;")
    putStrLn $ "Test 3: " ++ show (lexer "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)")
    putStrLn $ "Test 4: " ++ show (lexer "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;")
    putStrLn $ "Test 5: " ++ show (lexer "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;")
    putStrLn $ "Test 6: " ++ show (lexer "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);")
    putStrLn $ "Test 7: " ++ show (lexer "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);")

testLexer :: String -> [Token]
testLexer programCode = lexer programCode


