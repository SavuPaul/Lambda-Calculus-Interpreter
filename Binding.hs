module Binding where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- 3.1.
{-
    replaceMacro :: Context -> Lambda -> Lambda
    replaceMacro context (Var x) = Var x
    replaceMacro context (App e1 e2) = App (replaceMacro context e1) (replaceMacro context e2)
    replaceMacro context (Abs x e) = (Abs x (replaceMacro context e))
    replaceMacro context (Macro macro) = case lookup macro context of
                                            Just macro2 -> replaceMacro context macro2
                                            Nothing -> error ""
-}

{-
    This function returns Either String Lambda just like the simplifyCtx function
    because the case with Left "Error" has to be handled.
-}
replaceMacro :: Context -> Lambda -> Either String Lambda
replaceMacro context (Var x) = Right (Var x)

replaceMacro context (App e1 e2) = 
    case replaceMacro context e1 of
        Right e11 -> case replaceMacro context e2 of
            Right e22 -> Right (App e11 e22)
            Left "Error" -> Left "Error"
        Left "Error" -> Left "Error"    

replaceMacro context (Abs x e0) = 
    case replaceMacro context e0 of
        Right e00 -> Right (Abs x e00)
        Left "Error" -> Left "Error"

replaceMacro context (Macro macro) = case lookup macro context of
                                        Just macro2 -> replaceMacro context macro2
                                        Nothing -> Left "Error"                                

{-
    The replaceMacro function is used to replace all macros within the expression
    Based on what the function returns, we propagate the error or return the result
    by calling the simplify method with the given f function as a parameter
-}
simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx f expr = case replaceMacro ctx expr of
    Left "Error" -> Left "Error"
    Right lambda -> Right (simplify f lambda)

normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
