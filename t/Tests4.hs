import Parser
import Lexer

main :: IO ()
main = do
    -- putStrLn $ "Test 0: " ++ show (parseB (lexer ""))
    -- putStrLn $ "Test 1: " ++ show (parseB (lexer "True"))
    -- putStrLn $ "Test 2: " ++ show (parseB (lexer "True and True"))
    -- putStrLn $ "Test 3: " ++ show (parseB (lexer "True = False"))
    -- putStrLn $ "Test 4: " ++ show (parseB (lexer "True = False and True"))
    -- putStrLn $ "Test 5: " ++ show (parseB (lexer "1 <= 2"))
    -- putStrLn $ "Test 6: " ++ show (parseB (lexer "1 <= 2 and 3 <= 4"))
    -- putStrLn $ "Test 7: " ++ show (parseB (lexer "1 == 2 = 3 == 4"))
    -- putStrLn $ "Test 8: " ++ show (parseB (lexer "1 == 2"))
    -- putStrLn $ "Test 9: " ++ show (parseB (lexer "not True"))
    putStrLn $ "Test 11: " ++ show (parseB [T_not,T_bool True,T_and,T_integer 2,T_leq,T_integer 5,T_beq,T_integer 3,T_aeq,T_integer 4])
    putStrLn $ "Test 12: " ++ show (parseStatements [T_var "x",T_assign,T_integer 1])
    putStrLn $ "Test 13: " ++ show (parseStatements [T_var "y",T_assign,T_integer 2])

