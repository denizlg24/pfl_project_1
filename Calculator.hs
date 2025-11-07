{-
  A basic calculator for arithmetic expressions
  Based on the example in Chapter 8 of "Programming in Haskell"
  by Graham Hutton.

  Pedro Vasconcelos, 2025
-}
module Main where

import Parsing
import Data.Char

--
-- a data type for expressions
-- made up from integer numbers, + and *
--
type Name = String
data Expr = Num Integer
          | Var Name
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | IDiv Expr Expr
          | IMod Expr Expr
          deriving Show

-- a recursive evaluator for expressions
--

eval :: Env -> Expr -> Integer
eval env (Num n) = n
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (IDiv e1 e2) = eval env e1 `div` eval env e2
eval env (IMod e1 e2) = eval env e1 `mod` eval env e2
eval env (Var v) = case lookup v env of
                      Just val -> val
                      Nothing -> error ("undefined variable: " ++ v)

type Env = [( Name , Integer )]
type Command = ( Name , Expr )
-- | a parser for expressions
-- Grammar rules:
--
-- expr ::= term exprCont
-- exprCont ::= '+' term exprCont | epsilon

-- term ::= factor termCont
-- termCont ::= '*' factor termCont | epsilon

-- factor ::= natural | '(' expr ')'

expr :: Parser Expr
expr = do t <- term
          exprCont t

exprCont :: Expr -> Parser Expr
exprCont acc = do char '+'
                  t <- term
                  exprCont (Add acc t)
               <|> 
               do char '-'
                  t <- term
                  exprCont (Sub acc t)
               <|> return acc
              
              
term :: Parser Expr
term = do f <- factor
          termCont f

termCont :: Expr -> Parser Expr
termCont acc =  do char '*'
                   f <- factor  
                   termCont (Mul acc f)
                 <|>
                do char '/'
                   f <- factor
                   termCont (IDiv acc f)
                 <|>
                do char '%'
                   f <- factor
                   termCont (IMod acc f)
                 <|> return acc

factor :: Parser Expr
factor = do n <- natural
            return (Num n)
          <|>
         do v <- variable
            return (Var v)
          <|>
          do char '('
             e <- expr
             char ')'
             return e

command :: Parser Command
command = do v <- variable
             char '='
             e <- expr
             return (v,e)    
          <|>
          do e <- expr
             return ("",e)         

natural :: Parser Integer
natural = do xs <- many1 (satisfy isDigit)
             return (read xs)

variable :: Parser String
variable = do xs <- many1 (satisfy isAlpha)
              return (read xs)         

----------------------------------------------------------------             
  
main :: IO ()
main
  = do txt <- getContents
       calculator (lines txt)

-- | read-eval-print loop
calculator :: [String] -> IO ()
calculator []  = return ()
calculator (l:ls) = do 
                          putStrLn (evaluate [] l)
                          calculator ls  

-- | evaluate a single expression
evaluate :: Env -> String -> String
evaluate env txt
  = case parse expr txt of
      [ (tree, "") ] ->  show (eval env tree)
      _ -> "parse error; try again"  