module Default where

import Lambda
import Binding

-- Variables (for convenience)
vx = Var "x"
vy = Var "y"
vz = Var "z"
vf = Var "f"
vg = Var "g"
vh = Var "h"
vm = Var "m"
vn = Var "n"

-- Basic combinators
m = Abs "x" $ App vx vx
i = Abs "x" $ vx
k = Abs "x" $ Abs "y" $ vx
ki = Abs "x" $ Abs "y" $ vy
c = Abs "x" $ Abs "y" $ Abs "z" $ App (App vx vz) vy
y = Abs "f" $ App fix fix
  where fix = Abs "x" $ App vf (App vx vx)

-- 4.1. Boolean encodings
-- λx.λy.x
bTrue = Abs "x" $ Abs "y" $ vx
-- λx.λy.y
bFalse = Abs "x" $ Abs "y" $ vy
-- λx.λy.((x y) x)
bAnd = Abs "x" $ Abs "y" $ App (App vx vy) vx
-- λx.λy.((x x) y)
bOr = Abs "x" $ Abs "y" $ App (App vx vx) vy
-- λx.((x FALSE) TRUE)
bNot = Abs "x" $ App (App vx bFalse) bTrue
-- λx.λy.(OR ((AND x) (NOT y)) ((AND (NOT x)) y))
bXor = Abs "x" $ Abs "y" $ App (App bOr (App (App bAnd vx) (App bNot vy))) (App (App bAnd (App bNot vx)) vy)

-- 4.2. Pair encodings
-- λx.λy.λz.((z x) y)
pair = Abs "x" $ Abs "y" $ Abs "z" $ App (App vz vx) vy
-- λx.(x TRUE)
first = Abs "x" $ App vx bTrue
-- λx.(x FALSE)
second = Abs "x" $ App vx bFalse

-- 4.3. Natural number encodings
-- n0 = λf.λx.x
-- n1 = λf.λx.(f x)
-- n2 = λf.λx.(f (f x))
n0 = Abs "f" $ Abs "x" vx
n1 = Abs "f" $ Abs "x" $ App vf vx
n2 = Abs "f" $ Abs "x" $ App vf (App vf vx)
-- SUCC = λn.λf.λx.(f ((n f) x))
nSucc = Abs "n" $ Abs "f" $ Abs "x" $ App vf (App (App vn vf) vx)
-- PRED = λn.λf.λx.(((n (λg.λh.(h (g f)))) λu.x) λv.v)
nPred = Abs "n" $ Abs "f" $ Abs "x" $ App (App (App vn (Abs "g" $ Abs "h" $ App vh (App vg vf))) (Abs "u" vx)) (Abs "y" vy)
-- ADD = λn.λm.λf.λx.((n f) ((m f) x))
nAdd = Abs "n" $ Abs "m" $ Abs "f" $ Abs "x" $ App (App vn vf) (App (App vm vf) vx)
-- SUB = λm.λn.((n pred) m)
nSub = Abs "m" $ Abs "n" $ App (App vn nPred) vm
-- nmult = λm.λn.λf.λx.((m (n f)) x)
nMult = Abs "m" $ Abs "n" $ Abs "f" $ Abs "x" $ App (App vm (App vn vf)) vx

-- Default Context
defaultContext :: Context
defaultContext = 
    [ ("M", m)
    , ("I", i)
    , ("K", k)
    , ("KI", ki)
    , ("C", c)
    , ("Y", y)
    , ("TRUE", bTrue)
    , ("FALSE", bFalse)
    , ("AND", bAnd)
    , ("OR", bOr)
    , ("NOT", bNot)
    , ("XOR", bXor)
    , ("PAIR", pair)
    , ("FST", first)
    , ("SND", second)
    , ("N0", n0)
    , ("N1", n1)
    , ("N2", n2)
    , ("SUCC", nSucc)
    , ("PRED", nPred)
    ,("ADD", nAdd)
    , ("SUB", nSub)
    , ("MULT", nMult)
    ]
