import Compiler
import Parser
import Lexer

main :: IO ()
main = do
    putStrLn $ "\n\nTest 1"
    putStrLn $ "\n\nparsed: " ++ show (parse "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;")
    putStrLn $ "\n\nparsed: " ++ show (compile $ parse "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;")
    putStrLn $ "\n___________________________//___________________________\n"
    putStrLn $ "\n\nTest 2"
    putStrLn $ "\n\nparsed: " ++ show (parse "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;")
    putStrLn $ "\n\nparsed: " ++ show (compile $ parse "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;")

{-
Test 1

x := 42; 
if x <= 43 
then x := 1; 
else x := 33; 
x := x+1;

parsed: 
[
    S_assign "x" (A_num 42),
    S_if (B_leq (A_var "x") (A_num 43)) 
    [
        S_assign "x" (A_num 1)
    ] 
    [
        S_assign "x" (A_num 33)
    ]
]


parsed: 
[
    Push 42,Store "x",
    Push 43,Fetch "x",Le,Branch 
    [
        Push 1,Store "x"
    ] 
    [
        Push 33,Store "x"
    ]
]

___________________________//___________________________



Test 2

x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;

parsed: [S_assign "x" (A_num 42),S_if (B_leq (A_var "x") (A_num 43)) [S_assign "x" (A_num 1)] [S_assign "x" (A_num 33)],S_assign "z" (A_add (A_var "x") (A_var "x"))]


parsed: [Push 42,Store "x",Push 43,Fetch "x",Le,Branch [Push 1,Store "x"] [Push 33,Store "x"],Fetch "x",Fetch "x",Add,Store "z"]
-}