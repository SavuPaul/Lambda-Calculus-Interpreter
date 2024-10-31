module Lambda where

{- 
  nub - removes dublicates from a list
  \\  - A \ B 
-}
import Data.List (nub, (\\))

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False


-- 1.1.
{- 
  Use nub to get rid of duplicates.
  For normal variables => [x]
  For applications => recursively call getVars for each expression
                      until normal variable cases are met
  For Abs x e => get the x variable appended to the list of variables
                 obtained from the recursive call for expression e
-}
vars :: Lambda -> [String]
vars expr = nub $ getVars expr
  where
    getVars (Var x) = [x]
    getVars (App e1 e2) = getVars e1 ++ getVars e2
    getVars (Abs x e) = x : getVars e


-- 1.2.
{-
  Use nub to get rid of duplicates
  The approach is to create an array where the elements x from Abs x e are added
  This way, we add an element only if that variable does NOT exist in the array,
  meaning that the element is free and not bounded
-}
freeVars :: Lambda -> [String]
freeVars expr = nub $ getFreeVars expr []
  where
    getFreeVars (Var x) variables = if x `elem` variables then [] else [x]
    getFreeVars (App e1 e2) variables = getFreeVars e1 variables ++ getFreeVars e2 variables
    getFreeVars (Abs x e) variables = getFreeVars e (x : variables)


-- 1.3.
{- 
  Generate alphabets recursively until a string is not found
  Start with the normal alphabet.
-}

newVar :: [String] -> String
newVar str = generateAlphabet (map (:[]) ['a'..'z'])
  where
    generateAlphabet alphabet =
      if null (alphabet \\ str)
        then generateAlphabet [a ++ b | a <- alphabet, b <- "":alphabet]
        else head (alphabet \\ str)

-- 1.4.
{-
  Var x is in normal form
  App e1 e2 is in normal form only if the components are in normal form and
    if the application isn't a redex itself
  Abs x e is in normal form only if e is in normal form
-}
isNormalForm :: Lambda -> Bool
isNormalForm expr = case expr of
  (Var x) -> True

  (App e1 e2) -> isNormalForm e1 && isNormalForm e2 && case App e1 e2 of
    (App (Abs _ _) _) -> False
    _ -> True

  (Abs x e) -> isNormalForm e


-- 1.5.
-- reduce to normal form
reduce :: String -> Lambda -> Lambda -> Lambda
{-
  reduce x x e => e
  reduce x y e => y
-}
reduce x (Var y) e = if x == y then e else Var y
{- 
  Replacing x in an application, with e is equivalent with an
  application where x is replaced with e in the components
  e1 and e2 of the initial application
-}
reduce x (App e1 e2) e = App (reduce x e1 e) (reduce x e2 e)
{-
  reduce x \x.e1 e = \x.e1 because the x's from e1 are bounded to the x from \x, not the outer x
  reduce x \y.e1 e => verify if expression e contains free variable y
    - If yes: use newVar to generate the first lexicographical non-existent 
      string from the reunion of the variables of expressions e1 and e and
      replace the initial y with the new string variable
    - If not: replace the occurences of x in e1 with e
-}
reduce x (Abs y e1) e =
  if x == y then Abs x e1
  else if y `elem` freeVars e 
    then Abs (newVar $ vars e ++ vars e1) (reduce x (reduce y e1 (Var (newVar $ vars e ++ vars e1))) e)
    else Abs y (reduce x e1 e)


-- 1.6.
{-
  Normal step for:
  - Var x is Var x
  - App e1 e2 is:
    - If e1 is Abs x expr => reduce x in expr with e2
    - If not, recursively call normalStep for e1. If e1 is in normalForm, call for e2
  - Abs x e, recursively call for e
  First evaluate the pattern of e1 because that is the outermost left possible expression
  to reduce.
-}
normalStep :: Lambda -> Lambda

normalStep (Var x) = Var x

normalStep (App e1 e2) =
  case e1 of
    Abs x expr -> reduce x expr e2
    _ -> if not (isNormalForm e1)
           then App (normalStep e1) e2
           else App e1 (normalStep e2)

normalStep (Abs x e) = Abs x (normalStep e)

-- 1.7.
{-
  Similar with normalStep except we have to start with evaluating
  whether the expressions are in normal form or not and THEN reduce
  because we want to reduce the innermost left expression
-}
applicativeStep :: Lambda -> Lambda

applicativeStep (Var x) = Var x

applicativeStep (App e1 e2) = 
  if not (isNormalForm e1) then App (applicativeStep e1) e2
  else if not (isNormalForm e2) then App e1 (applicativeStep e2)
  else case e1 of
        Abs x expr -> reduce x expr e2

applicativeStep (Abs x e) = Abs x (applicativeStep e)

-- 1.8.
{-
  applyFunc creates a list where the expression from every
  step is appended and then returns it in reverse order
-}
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify f expr = reverse $ applyFunc expr []
  where
    applyFunc expr list =
      if (isNormalForm expr)
        then (expr : list)
        else applyFunc (f expr) (expr : list)
      
    

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
