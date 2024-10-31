module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative
import Data.Char

import Lambda
import Binding

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

-- 2.1. / 3.2.
-- This parser fails and returns Nothing everytime
-- (Taken from course, adapted for Maybe)
failParser :: Parser a 
failParser = Parser $ \s -> Nothing

-- Parser when a condition is met
-- (Taken from course, adapted for Maybe)
predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $ \s -> 
                       case s of 
                        [] -> Nothing
                        (x:xs) -> if p x then Just (x,xs) else Nothing

-- Parser for characters
-- (Taken from course, adapted for Maybe)
charParser :: Char -> Parser Char
charParser c = Parser $ \s -> case s of
                            [] -> Nothing
                            (x : xs) -> if x == c then Just (x, xs) else Nothing

-- (Taken from course, adapted for Maybe)
instance Monad Parser where
    return v = Parser $ \s -> Just (v,s)
    mp >>= f = Parser $ \s -> case parse mp s of
                              Nothing -> Nothing
                              Just (v, rest) -> parse (f v) rest

-- (Taken from course)            
instance Alternative Parser where
    empty = failParser
    p1 <|> p2 = Parser $ \s -> case parse p1 s of
                                Nothing -> parse p2 s 
                                Just x -> Just x

-- (Taken from course)
instance Applicative Parser where
  af <*> mp = 
    do 
      f <- af
      v <- mp
      return $ f v
  pure = return

-- (Taken from course)
instance Functor Parser where 
  fmap f mp = 
    do 
      v <- mp
      return $ f v                           

-- (Taken from course)
plusParser :: Parser a -> Parser [a]
plusParser p = do
              x <- p
              xs <- starParser p
              return (x:xs)

-- (Taken from course)
starParser :: Parser a -> Parser [a]
starParser p = (plusParser p) <|> (return [])

-------------------------------------------------------------------------------------------

-- Parser for whitespaces (one or more)
-- (Taken from course)
whitespaceParser :: Parser String
whitespaceParser = starParser $ charParser ' '

-------------------------------------------------------------------------------------------

-- Parser for variables (which contain only alphabetic characters)
{- 
  The isAlphaNotUpper function verifies if the characters are not capitals. If they are, it
  means we're dealing with a Macro, not a Var. The Macro parser will take control.
-}
isAlphaNotUpper :: Char -> Bool
isAlphaNotUpper c = if (isAlpha c && (not (isUpper c))) then True else False

varParser :: Parser String
varParser = do 
            x <- plusParser (predicateParser isAlphaNotUpper)
            return x

-- Parser for Lambda variables (transforms the varParser data into an actual Var)
varLambdaParser :: Parser Lambda
varLambdaParser = Var <$> varParser     

--------------------------------------------------------------------------------------------

-- Parser for applications (applications can contain applications)
-- atomParser can parse any type of expression (Var, App, Abs, Macro)
appParser :: Parser Lambda
appParser = do
            c1 <- charParser '('
            lambda1 <- atomParser
            whitespaceParser
            lambda2 <- atomParser
            c2 <- charParser ')'
            return (App lambda1 lambda2)

--------------------------------------------------------------------------------------------

-- Parser for Abs expressions
-- atomParser can parse any type of expression (Var, App, Abs, Macro)
absParser :: Parser Lambda
absParser = do
            c1 <- charParser '\\'
            var <- varParser
            dot <- charParser '.'
            lambda <- atomParser
            return (Abs var lambda)       

--------------------------------------------------------------------------------------------

-- Parser for Macros
-- atomParser can parse any type of expression (Var, App, Abs, Macro)
-- macros are strings and can be parsed with varParser
macroParser :: Parser String
macroParser = do
              macro <- starParser (predicateParser isAlphaNum)
              return macro
          
macroLambdaParser :: Parser Lambda
macroLambdaParser = Macro <$> macroParser

--------------------------------------------------------------------------------------------

-- If varLambdaParser fails, use appParser
-- If appParser fails too, use absParser
-- If absParser fails too, use macroLambdaParser
atomParser :: Parser Lambda
atomParser = varLambdaParser <|> appParser <|> absParser <|> macroLambdaParser

--------------------------------------------------------------------------------------------

-- Lambda expression parser -> parses s with atomParser and uses pattern matching for Maybe
parseLambda :: String -> Lambda
parseLambda s = 
  case parse atomParser s of
    Just (lambda, "") -> lambda
    Just (lambda, rest) -> Var "ERROR"
    Nothing -> Var "ERROR"

--------------------------------------------------------------------------------------------

-- 3.3.
-- Eval parser
evalParser :: Parser Line
evalParser = do
             lambda <- atomParser
             return (Eval lambda)

--------------------------------------------------------------------------------------------

-- Binding parser
{-
  "MACRO=<definition>" (can contain whitespaces)
-}
bindingParser :: Parser Line
bindingParser = do
                macro <- macroParser
                whitespaceParser
                c <- charParser '='
                whitespaceParser
                lambda <- atomParser
                return (Binding macro lambda)

---------------------------------------------------------------------------------------------

lineParser :: Parser Line
lineParser = bindingParser <|> evalParser

parseLine :: String -> Either String Line
parseLine s = 
  case parse lineParser s of
    Just (lambda, "") -> Right lambda
    Just (lamda, rest) -> Left "Error"
    Nothing -> Left "Error"
