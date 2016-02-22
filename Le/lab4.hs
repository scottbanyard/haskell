-- lab 4 : implementing more things on lab 3

import Prelude hiding (Num)
import qualified Prelude (Num)
import Data.Set

type Num = Integer
type Var = String
-- 1b)
type Z = Integer
-- 1d) haskell type state as all poss functions from Var to Z
type State = Var -> Z

type T = Bool

data Aexp = N Num | V Var
    | Add Aexp Aexp | Mult Aexp Aexp | Sub Aexp Aexp | Nega Aexp -- 1h) added Neg to allow neg numbers
        deriving (Show, Eq, Read)

data Bexp = TRUE | FALSE
    | Eq Aexp Aexp | Le Aexp Aexp
    | Neg Bexp | And Bexp Bexp | Implies Bexp Bexp
        deriving (Show, Eq, Read)



-- 1a) expression of (x + y) * (z - 1)
a :: Aexp
a = Mult (Add (V "x") (V "y")) (Sub (V "z") (N 1))

-- 1c) haskell function from Num -> Z
nVal :: Num -> Z
nVal x = x

-- 1e) haskell term s :: State - sets state of x = 1, y = 2, z = 3, others to 0
s :: State
s "x" = 1
s "y" = 2
s "z" = 3
s _ = 0

-- 1f) function representing semantic function A[.] :: Aexp -> State -> Z for arithmetics
-- put in an Aexp and State and return a Z e.g. aVal a s returns 6
aVal :: Aexp -> State -> Z
aVal (N num) s = nVal num
aVal (V var) s = s var
aVal (Add exp1 exp2) s = (aVal exp1 s) + (aVal exp2 s)
aVal (Sub exp1 exp2) s = (aVal exp1 s) - (aVal exp2 s)
aVal (Mult exp1 exp2) s = (aVal exp1 s) * (aVal exp2 s)
aVal (Nega exp1) s = (aVal exp1 s) * (-1)
-- can't have A[-x] s = A[0 - -x] s as haskell doesn't support 0 -- x


{--
Q2) Write a concrete grammar for the arithmetic expressions of While that are
unambiguous.
--}

b :: Bexp
b = Neg ( Eq (Add (V "x") (V "y")) (N 4))

bVal :: Bexp -> State -> T
bVal TRUE s = True
bVal FALSE s = False
bVal (Eq exp1 exp2) s = (aVal exp1 s) == (aVal exp2 s)
bVal (Le exp1 exp2) s = (aVal exp1 s) <= (aVal exp2 s)
bVal (Neg exp1) s = not (bVal exp1 s)
bVal (And exp1 exp2) s = (bVal exp1 s) && (bVal exp2 s)
bVal (Implies exp1 exp2) s = (not (bVal exp1 s)) || (bVal exp2 s)

-- lab4;1d) bVal b s = True

b2 :: Bexp
b2 = Neg (Implies TRUE TRUE)

-- free variables in an arithmetic exp Aexp (variables in scope i.e. a + b -> a
-- and b are in scope, z isn't) using a set as the variables can occur at most once in the set
fvAexp :: Aexp -> Set Var
fvAexp (N num) = empty
fvAexp (V var) = singleton var
fvAexp (Add exp1 exp2) = fvAexp exp1 `union` fvAexp exp2
fvAexp (Sub exp1 exp2) = fvAexp exp1 `union` fvAexp exp2
fvAexp (Mult exp1 exp2) = fvAexp exp1 `union` fvAexp exp2
fvAexp (Nega exp1) = fvAexp exp1

-- free variables in a bool expression
fvBexp :: Bexp -> Set Var
fvBexp TRUE = empty
fvBexp FALSE = empty
fvBexp (Eq exp1 exp2) = fvAexp exp1 `union` fvAexp exp2
fvBexp (Le exp1 exp2) = fvAexp exp1 `union` fvAexp exp2
fvBexp (Neg exp1) = fvBexp exp1
fvBexp (And exp1 exp2) = fvBexp exp1 `union` fvBexp exp2
fvBexp (Implies exp1 exp2) = fvBexp exp1 `union` fvBexp exp2


{--
  6c) Prove that for every Boolean expression of While there is an equivalent
  Boolean expression in While

  The While language contains the logical signs "not" and "and" which are
  functionally complete - and so you can formulate any logical expression with them.
  P -> Q <=> ~P V Q
--}
