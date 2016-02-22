-- lab 3

import Prelude hiding (Num)
import qualified Prelude (Num)

type Num = Integer
type Var = String
-- 1b)
type Z = Integer
-- 1d) haskell type state as all poss functions from Var to Z
type State = Var -> Z

data Aexp = N Num | V Var
    | Add Aexp Aexp | Mult Aexp Aexp | Sub Aexp Aexp | Neg Aexp -- 1h) added Neg to allow neg numbers
        deriving (Show, Eq, Read)

-- 1a) expression of (x + y) * (z - 1)
a :: Aexp
a = Mult (Add (V "x") (V "y")) (Sub (V "z") (N 1))

-- 1c) haskell function from Num -> Z
n_val :: Num -> Z
n_val x = x

-- 1e) haskell term s :: State - sets state of x = 1, y = 2, z = 3, others to 0
s :: State
s "x" = 1
s "y" = 2
s "z" = 3
s _ = 0

-- 1f) function representing semantic function A[.] :: Aexp -> State -> Z for arithmetics
-- put in an Aexp and State and return a Z e.g. a_val a s returns 6
a_val :: Aexp -> State -> Z
a_val (N num) s = n_val num
a_val (V var) s = s var
a_val (Add exp1 exp2) s = (a_val exp1 s) + (a_val exp2 s)
a_val (Sub exp1 exp2) s = (a_val exp1 s) - (a_val exp2 s)
a_val (Mult exp1 exp2) s = (a_val exp1 s) * (a_val exp2 s)
a_val (Neg exp1) s = (a_val exp1 s) * (-1)
-- can't have A[-x] s = A[0 - -x] s as haskell doesn't support 0 -- x


{--
Q2) Write a concrete grammar for the arithmetic expressions of While that are
unambiguous.
--}
