import Parsing

-- expression parser

-- expr ::= term + expr | term
-- term ::= factor * term | factor
-- factor ::= ( expr) | int

-- Exercise: modify to return ArithExpr rather than Int
-- Exercise: Add parser for variables (lower case Char)

expr :: Parser Int
expr = do x <- term
          symbol "+"
          y <- expr
          return (x+y)
       <|> term

term :: Parser Int
term = do x <- factor
          symbol "*"
          y <- term
          return (x*y)
       <|> factor

factor :: Parser Int
factor = do symbol "("
            x <- expr
            symbol ")"
            return x
        <|> natural

eval :: String -> Int
eval xs = case (parse expr xs) of
           [(n,[])]  -> n
           [(_,out)] -> error ("Unused input " ++ out)
           []        -> error "Invalid input"
