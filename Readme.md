# Functional and Logical Programming 

## 2nd Practical Project - Haskell

> This project was divided into two halfs, and so we decided to also divide the report into two halfs. 
> Descriptions and explanation of our thought process as well how we solved the problems are included in their respective part.


## Team

Group: &nbsp;&nbsp;`T06_G13`

<table>
<tr><th>Name</th><th>Student Number</th><th>Contribution (%)</th></tr>
<tr><td>Diogo Tomás Valente Fernandes</td><td>up202108752</td><td>Over 9000</td></tr>
<tr><td>Hélder Gabriel Silva Costa</td><td>up202108719</td><td>Over 9000</td></tr>
</table>


## Part 1

### Description

For the first half the challenge was 
to consider a low-level machine with configurations of the form (c, e, s) where 
c is a list of instructions (or code) to be executed, e is the evaluation stack,
and s is the storage, and the following instructions push-n, add, mult, sub, true,
false, eq, le, and, neg, fetch-x, store-x, noop, branch(c1, c2) and loop(c1, c2).

In order to complete this, first we started by developing two new types, one to represent
the machine's state and another to represent the machine's stack. These were done in
the *Stack.hs* and *State.hs* files.

For the Stack this is the code that defines the new data type as well as the constructor
for an empty stack:

```hs
-- Definition of new type for a stack of values.
newtype Stack = Stack [Val]

-- Function to create an empty stack.
createEmptyStack :: Stack
createEmptyStack = Stack []
```

For the machine's state this is the code that defines the new data type as well as the constructor for a new (empty) State. The code that defines a variable is also included:

```hs
-- Definition of the new type Var (variable)
type Var = String

-- Definition for the machine's State.
type State = Map.Map Var Val

-- Function to create an empty state.
createEmptyState :: State
createEmptyState = Map.empty
```

After this we developed the *stack2Str* function that is responsible for transforming the given stack to a string following the restrictions given in the project assignment paper. This is it's respective code:

```hs
-- Function responsible for converting the stack to a string.
stack2Str :: Stack -> String
stack2Str (Stack xs) = intercalate "," (map value2Str xs)
```

The same was done for the state with the *state2Str* function. We also made an helper function for tranforming a variable-value pair to a string. Here's the respective code:

```hs
-- Function to convert a state to a string.
state2Str :: State -> String
state2Str state
  | Map.null state = ""
  | otherwise = intercalate "," (map entryToStr (Map.toList state))

-- Function to convert a variable-value pair to a string.
entryToStr :: (Var, Val) -> String
entryToStr (var, val) = var ++ "=" ++ value2Str val
```

Finally, and the most challenging part of this half, we needed to develop an interpreter for programs in this machine that takes a list of instructions, an empty and a state and runs the given instructions returning an empty code list, a stack and the output values in the state.

For this in the *Inst.hs* file, firstly we defined a new data type that corresponds to individual instructions as well as an alias (Code) for a list of instructions. All the instructions suggested in the assignment were included, here's the code:

```hs
-- Definition of the 'Inst' data type representing individual instructions.
data Inst =
    Push Integer      -- Pushes an integer onto the stack
  | Add               -- Addition operation
  | Mult              -- Multiplication operation
  | Sub               -- Subtraction operation
  | Tru               -- Pushes True to the stack
  | Fals              -- Pushes False to the stack
  | Equ               -- Equality comparison
  | Le                -- Less than or equal comparison
  | And               -- Logical AND operation
  | Neg               -- Logical NOT operation
  | Fetch String      -- Fetches the value of a variable from the state
  | Store String      -- Stores a value into a variable in the state
  | Noop              -- No-operation
  | Branch Code Code  -- Conditional branching based on the top of the stack
  | Loop Code Code    -- Loop construction
  deriving Show

-- Definiton of alias for a list of instructions.
type Code = [Inst]
```

After this in the *Interpreter.hs* file we developed the **exec** function that is responsible for executing a single step (instruction), in that function we also implement all the code needed for the instructions to work. Here's the function:

```hs
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
```

During the development of this function we defined some auxiliary functions in the *State* and *Stack* files, those were pushing a variable-value pair into the state, getting the value of a variable from the state, pushing a value on to the stack, popping a value from the stack and returning the stack's top value, here are the respective codes:

```hs
-- Function to push a variable-value pair into the state.
push :: State -> Var -> Val -> State
push state var val = Map.insert var val state

-- Function to get the value of a variable from the state.
get :: State -> Var -> Maybe Val
get state var = Map.lookup var state
```

```hs
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
```

Finally we declared the function to run the interpreter, again following the restrictions in the assignment paper. Here's the respective code:

```hs
-- Function to run the interpreter.
run :: (Code, Stack, State) -> (Code, Stack, State)
run (code, stack, state) = 
    case exec (code, stack, state) of
        Just nextInterpreter -> run nextInterpreter
        Nothing -> (code, stack, state)
```

In order to test our code we ran it through several test cases, and got positive results in all of them, here are the tests we ran:

Nsei se deviamos incluir idk, são imensos testes.
```hs
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

testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)
```

And the corresponding results:

![Test Results](images/Tests1.png)

## Part 2

### Description

For the second half the challenge was to consider a small imperative programming language with arithmetic and boolean expressions, and statements consisting of assignments of the form x := a, sequence of statements (instr1 ; instr2), if then else statements, and while loops, and develop a compiler from this language into lists of instructions in the previous machine.


In order to complete this, we started by declaring three new data types, *Aexp* for arithmetic expression, *Bexp* for boolean expression and *Stm* for statements. We also defined an alias for a list of statements, called a Program. Here's the respective code:

```hs
-- Data type for arithmetic expressions.
data Aexp
    = A_var String           -- Variable
    | A_num Integer          -- Integer constant
    | A_add Aexp Aexp        -- Addition
    | A_sub Aexp Aexp        -- Subtraction
    | A_mul Aexp Aexp        -- Multiplication
    deriving (Eq, Show)

-- Data type for boolean expressions.
data Bexp
    = B_true                 -- True
    | B_false                -- False
    | B_aeq Aexp Aexp        -- Arithmetic equality
    | B_beq Bexp Bexp        -- Boolean equality
    | B_leq Aexp Aexp        -- Less than or equal to
    | B_not Bexp             -- Logical NOT
    | B_and Bexp Bexp        -- Logical AND
    deriving (Eq, Show)

-- Data type for statements.
data Stm
    = S_assign String Aexp   -- Assignment statement
    | S_if Bexp [Stm] [Stm]  -- If statement with true and false branches
    | S_while Bexp [Stm]     -- While loop
    deriving (Eq, Show)

-- Alias definition for a program, which is a list of statements.
type Program = [Stm]
```

After we developed the compiler that transform a program from this imperative language into machine instructions for the machine in the first half. The syntax enforced in the assignment paper was followed and here's the resulting code included in the *compiler.hs* file:

```hs
-- Function to compile arithmetic expressions (Aexp) to instructions (Code).
compA :: Aexp -> Code
compA (A_var varName) = [Fetch varName]                             -- Fetch value from a variable
compA (A_num num) = [Push num]                                      -- Push an integer on to the stack
compA (A_add aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Add]     -- Addition
compA (A_sub aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Sub]     -- Subtraction
compA (A_mul aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Mult]    -- Multiplication

-- Function to compile boolean expression (Bexp) to instructions (Code).
compB :: Bexp -> Code
compB (B_true) = [Tru]                                              -- Push True on to the stack
compB (B_false) = [Fals]                                            -- Push False on to the stack
compB (B_aeq aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Equ]     -- Arithmetic equality
compB (B_beq bexp1 bexp2) = compB bexp2 ++ compB bexp1 ++ [Equ]     -- Boolean equality
compB (B_leq aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Le]      -- Less than or equal
compB (B_not bexp1) = compB bexp1 ++ [Neg]                          -- Boolean negation
compB (B_and bexp1 bexp2) = compB bexp1 ++ compB bexp2 ++ [And]     -- Boolean AND

-- Function to compile a program to instructions (Code).
compile :: Program -> Code
compile [] = []                                                                 -- Empty program returns empty list of instructions
compile (S_assign var aexp : prog) = compA aexp ++ [Store var] ++ compile prog  -- Compile arithmetic expression
compile (S_if bexp stm1 stm2 : prog) =                                          -- Compile Branch with condtional execution
    let
        on_true = compile stm1
        on_false = compile stm2
        condition = compB bexp
    in
        condition ++ [Branch on_true on_false] ++ compile prog
compile (S_while bexp stm : prog) =                                             -- Compile Loop with condition execution 
    let
        on_true = compile stm
        condition = compB bexp
    in
        [Loop on_true condition] ++ compile prog

```

Finally, we needed to develop a parser that transforms the program given as a string into Statements. For that, we developed a lexer that takes the given string and splits it into tokens facilitating the following parsing functions. We also developed functions that transform variables and integer to their associated tokens. Here's the code for the lexer included in the *lexer.hs* file:

```hs
-- Defining the 'Token' data type that corresponds that serve as the input to the parsing stage.
data Token
    = T_while                 -- | while
    | T_do                    -- | do
    | T_if                    -- | if
    | T_then                  -- | then
    | T_else                  -- | else
    | T_not                   -- | not
    | T_plus                  -- | +
    | T_less                  -- | -
    | T_times                 -- | *
    | T_semicolon             -- | ;
    | T_lbracket              -- | (
    | T_rbracket              -- | )
    | T_assign                -- | :=
    | T_leq                   -- | <=
    | T_aeq                   -- | ==
    | T_beq                   -- | =
    | T_and                   -- | and
    | T_integer Integer       -- | Number
    | T_bool Bool             -- | Boolean
    | T_var String            -- | Variable
    deriving (Eq, Show)


-- Function that transforms a string to a list of tokens.
lexer :: String -> [Token]
lexer [] = []
lexer ('w':'h':'i':'l':'e':restStr) = T_while : lexer restStr
lexer ('d':'o':restStr) = T_do : lexer restStr
lexer ('i':'f':restStr) = T_if : lexer restStr
lexer ('t':'h':'e':'n':restStr) = T_then : lexer restStr
lexer ('e':'l':'s':'e':restStr) = T_else : lexer restStr
lexer ('n':'o':'t':restStr) = T_not : lexer restStr
lexer ('+':restStr) = T_plus : lexer restStr
lexer ('-':restStr) = T_less : lexer restStr
lexer ('*':restStr) = T_times : lexer restStr
lexer (';':restStr) = T_semicolon : lexer restStr
lexer ('(':restStr) = T_lbracket : lexer restStr
lexer (')':restStr) = T_rbracket : lexer restStr
lexer (':':'=':restStr) = T_assign : lexer restStr
lexer ('<':'=':restStr) = T_leq : lexer restStr
lexer ('=':'=':restStr) = T_aeq : lexer restStr
lexer ('=':restStr) = T_beq : lexer restStr
lexer ('a':'n':'d':restStr) = T_and : lexer restStr
lexer ('T':'r':'u':'e':restStr) = T_bool True : lexer restStr
lexer ('F':'a':'l':'s':'e':restStr) = T_bool False : lexer restStr
lexer (chr:restStr)
    | isSpace chr = lexer restStr
    | isDigit chr = lexNum (chr:restStr)
    | isAlpha chr = lexVar (chr:restStr)
    | otherwise = error ("lexer: unexpected character " ++ [chr])

-- Function responsible for transforming a integer to its respective token.
lexNum :: String -> [Token]
lexNum str = T_integer (read numStr) : lexer restStr
    where (numStr, restStr) = span isDigit str

-- Function responsible for transforming a variable to its respective token.
lexVar :: String -> [Token]
lexVar str = T_var varStr : lexer restStr
    where (varStr, restStr) = span isAlpha str
```

In order to develop the parser we decided to divide it into two parsers, *parseA* responsible for parsing arithmetical expressions and taking into account the arithmetic precedences, and *parseB* responsible for parsing boolean expressions taking into account it's own precedences and it may call *parseA*, in case there are arithmetical expressions nested into the boolean expression. Both these are included in the *parser.hs* file.

Here's the code for *parseA*:

```hs
-- Function to parse an arithmetic expression (Aexp) from a list of tokens.
parseA :: [Token] -> Aexp
parseA tokens =
    case parseSumOrSubOrProdOrIntOrVarOrPar tokens of
        Just (expr, []) -> expr
        _ -> error "Run-time error"

-- Parsing integer constants, variables, or expressions within parentheses.
-- Int Var Par
parseIntOrVarOrPar :: [Token] -> Maybe (Aexp, [Token])
parseIntOrVarOrPar (T_var var : restTokens)
    = Just (A_var var, restTokens)
parseIntOrVarOrPar (T_integer num : restTokens)
    = Just (A_num num, restTokens)
parseIntOrVarOrPar (T_lbracket : restTokens1)
    = case parseSumOrSubOrProdOrIntOrVarOrPar restTokens1 of
        Just (expr, (T_rbracket : restTokens2)) ->
            Just (expr, restTokens2)
        Just _ -> Nothing -- no closing paren
        Nothing -> Nothing
parseIntOrVarOrPar tokens = Nothing

-- Parsing multiplication, integer constants, variables, or expressions within parentheses.
-- Prod Int Var Par
parseProdOrIntOrVarOrPar :: [Token] -> Maybe (Aexp, [Token])
parseProdOrIntOrVarOrPar tokens
    = case parseIntOrVarOrPar tokens of
        Just (expr1, (T_times : restTokens1)) ->
            case parseProdOrIntOrVarOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (A_mul expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result

-- Parsing addition, subtraction, multiplication, integer constants, variables, or expressions within parentheses.
-- Sum Sub Prod Int Var Par
parseSumOrSubOrProdOrIntOrVarOrPar::[Token] -> Maybe (Aexp, [Token])
parseSumOrSubOrProdOrIntOrVarOrPar tokens
    = case parseProdOrIntOrVarOrPar tokens of
        Just (expr1, (T_plus : restTokens1)) ->
            case parseSumOrSubOrProdOrIntOrVarOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (A_add expr1 expr2, restTokens2)
                Nothing -> Nothing
        Just (expr1, (T_less : restTokens1)) ->
            case parseSumOrSubOrProdOrIntOrVarOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (A_sub expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result
```

And the code for *parseB*:

```hs
-- Function to parse a boolean expression (Bexp) from a list of tokens.
parseB :: [Token] -> Bexp
parseB tokens =
    case parseAndOrBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar tokens of
        Just (expr, []) -> expr
        _ -> error "Run-time error"

-- Parsing True, False or expressions withing parentheses.
-- True False Par
parseTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseTrueOrFalseOrPar (T_bool True : restTokens)
    = Just (B_true, restTokens)
parseTrueOrFalseOrPar (T_bool False : restTokens)
    = Just (B_false, restTokens)
parseTrueOrFalseOrPar (T_lbracket : restTokens1)
    = case parseAndOrBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar restTokens1 of
        Just (expr, (T_rbracket : restTokens2)) ->
            Just (expr, restTokens2)
        Just _ -> Nothing -- no closing paren
        Nothing -> Nothing
parseTrueOrFalseOrPar tokens = Nothing

-- Parsing less than or equal (Leq) expressions, True, False, or expressions within parentheses.
-- Leq True False Par
parseLeqOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseLeqOrTrueOrFalseOrPar tokens
    = case parseSumOrSubOrProdOrIntOrVarOrPar tokens of
        Just (expr1, (T_leq : restTokens1)) ->
            case parseSumOrSubOrProdOrIntOrVarOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (B_leq expr1 expr2, restTokens2)
                Nothing -> Nothing
        _ -> parseTrueOrFalseOrPar tokens

-- Parsing arithmetic equality (Aeq), less than or equal (Leq), True, False, or expressions within parentheses.
-- Aeq Leq True False Par
parseAeqOrLeqOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseAeqOrLeqOrTrueOrFalseOrPar tokens
    = case parseSumOrSubOrProdOrIntOrVarOrPar tokens of
        Just (expr1, (T_aeq : restTokens1)) ->
            case parseSumOrSubOrProdOrIntOrVarOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (B_aeq expr1 expr2, restTokens2)
                Nothing -> Nothing
        _ -> parseLeqOrTrueOrFalseOrPar tokens

-- Not Aeq Leq True False Par
-- parseNotOrAeqOrLeqOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
-- parseNotOrAeqOrLeqOrTrueOrFalseOrPar tokens
--     = case parseAeqOrLeqOrTrueOrFalseOrPar tokens of
--         Just (expr1, (T_not : restTokens1)) ->
--             case parseNotOrAeqOrLeqOrTrueOrFalseOrPar restTokens1 of
--                 Just (expr2, restTokens2) ->
--                     Just (B_not expr2, restTokens2)
--                 Nothing -> Nothing
--         a -> error $ "error: " ++ show a 
--         Just (expr1, _) -> error $ "Expr: " ++ show expr1
--         result -> result

-- Parsing boolean negation (Not), arithmetic equality (Aeq), less than or equal (Leq), True, False, or expressions within parentheses.
-- Not Aeq Leq True False Par
parseNotOrAeqOrLeqOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseNotOrAeqOrLeqOrTrueOrFalseOrPar (T_not : restTokens)
    -- = error $ "rest: " ++ show restTokens
    = case parseAeqOrLeqOrTrueOrFalseOrPar restTokens of
        Just (expr1, restTokens1) ->
            Just (B_not expr1, restTokens1)
        -- result -> error "AMDIASJDIJA IODJAISO JDOAJD OIASJDIO JASSOIDJ AS"
        result -> parseAeqOrLeqOrTrueOrFalseOrPar restTokens
parseNotOrAeqOrLeqOrTrueOrFalseOrPar tokens = parseAeqOrLeqOrTrueOrFalseOrPar tokens

-- Parsing boolean equality (Beq), boolean negation (Not), arithmetic equality (Aeq),
-- less than or equal (Leq), True, False, or expressions within parentheses.
-- Beq Not Aeq Leq True False Par
parseBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar tokens
    = case parseNotOrAeqOrLeqOrTrueOrFalseOrPar tokens of
        Just (expr1, (T_beq : restTokens1)) ->
            case parseBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (B_beq expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result

-- Parsing logical AND (And), boolean equality (Beq), boolean negation (Not), arithmetic equality (Aeq),
-- less than or equal (Leq), True, False, or expressions within parentheses.
-- And Beq Not Aeq Leq True False Par
parseAndOrBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar :: [Token] -> Maybe (Bexp, [Token])
parseAndOrBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar tokens
    = case parseBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar tokens of 
        Just (expr1, (T_and : restTokens1)) ->
            case parseAndOrBeqOrNotOrAeqOrLeqOrTrueOrFalseOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (B_and expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result
```