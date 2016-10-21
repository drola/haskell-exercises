module Calculator where

data CharType = Symbol Char | Whitespace | Digit Char deriving Show

toCharType :: Char -> CharType

toCharType '+' = Symbol '+'
toCharType '-' = Symbol '-'
toCharType '*' = Symbol '*'
toCharType '/' = Symbol '/'
toCharType '(' = Symbol '('
toCharType ')' = Symbol ')'

toCharType ' ' = Whitespace

toCharType '0' = Digit '0'
toCharType '1' = Digit '1'
toCharType '2' = Digit '2'
toCharType '3' = Digit '3'
toCharType '4' = Digit '4'
toCharType '5' = Digit '5'
toCharType '6' = Digit '6'
toCharType '7' = Digit '7'
toCharType '8' = Digit '8'
toCharType '9' = Digit '9'
toCharType '.' = Digit '.'


data Token = TPlus | TMinus | TMultiply | TDivide | TLGroup | TRGroup | TNumber [Char] | TBreak deriving Show

symbolToToken :: Char -> Token
symbolToToken '+' = TPlus
symbolToToken '-' = TMinus
symbolToToken '*' = TMultiply
symbolToToken '/' = TDivide
symbolToToken '(' = TLGroup
symbolToToken ')' = TRGroup

--Symbols
nextChar :: [Token] -> CharType -> [Token]
nextChar ts (Symbol x) = [symbolToToken x] ++ ts
--Breaks (whitespace)
nextChar  (TBreak:ts) Whitespace = [TBreak] ++ ts --This joins multiple breaks in succession into one
nextChar  ts Whitespace = [TBreak] ++ ts
--Numbers
nextChar ((TNumber n):ts) (Digit d) = [TNumber $ [d] ++ n] ++ ts
nextChar ts (Digit d) = [TNumber [d]] ++ ts

tokenize :: [Char] -> [Token]
tokenize cs = reverse
    $ foldl nextChar []
    $ map toCharType cs


--Parse: Transform tokens into expression trees

--Applying associativity rules will reduce number of [Token] cases and increase depth of the tree.
--If everything is correct, there should be either one or three tokens in each leaf of the tree
data TokenTree = TTT [TokenTree] | TT [Token]



parseNumber :: TNumber -> Expression
parseNumber n = Constant 1 --TODO

--How to apply associativity rules?
parse :: [Token] -> Expression
parse [TNumber n] = Constant 1
parse [TMultiply]



data OperationSymbol = OpPlus | OpMinus | OpMultiply | OpDivide deriving Show;

data Expression =
    Constant Float |
    Operation2 OperationSymbol Expression Expression
    deriving Show


evaluateOperation :: Fractional a => OperationSymbol -> a -> a -> a
evaluateOperation OpPlus a b = a + b
evaluateOperation OpMinus a b = a - b
evaluateOperation OpMultiply a b = a * b
evaluateOperation OpDivide a b = a / b

-- Example: evaluate $ Operation2 OpPlus (Constant 2) (Constant 3)
evaluate :: Expression -> Float
evaluate (Constant x) = x
evaluate (Operation2 op a b) = evaluateOperation op (evaluate a) (evaluate b)

