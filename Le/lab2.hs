-- 1a)
data Bit = On | Off deriving (Eq, Ord, Enum, Read)
-- b) : B and S are type constuctors where Bit is the type, and Bit Word recursive type
data Word = B Bit | S Bit Word

-- On replaced with 1, Off replaced with 0
instance Show Bit where
  show (On) = "1"
  show (Off) = "0"

-- examples of a Bit b and a Word c/d
-- 1
b = On
-- 101
c = S On (S Off (B On))
-- 10
d = S On (B Off)

-- changes Word format by changing c to 101
instance Show Word where
  show (B b) = show b
  show (S b s) = show b ++ show s


-- c) function that computes integer value represented from Word
wordToInt :: Word -> Integer
wordToInt (S On s) = 1 + (2 * wordToInt s)
wordToInt (S Off s) = (2 * wordToInt s)
wordToInt (B On) = 1
wordToInt (B Off) = 0

----------------------------------------------------------------

-- 2a)
data Num = Integer
data Var = String

-- b)
data Aexp = N Main.Num | V Var
  | Add Aexp Aexp | Mult Aexp Aexp | Sub Aexp Aexp

data Bexp = TRUE | FALSE
  | Eq Aexp Aexp | Le Aexp Aexp
  | Neg Bexp | And Bexp Bexp

data Stm = Assign Var Aexp | Skip | CompStm Stm
  | If Bexp Stm Stm
  | While Bexp Stm

-- c)
--p :: Stm
{-|p =
  (Comp
    (Assign "z" (V "x"))
      (While
        (Le (N 2) (V "y"))
          (Comp
            (Assign "z" (Mult (V "x") "z"))
              (Assign "y" (Sub ("y") (N 1)))))
              -}

{-|
  3) loop invariant
  Add Pre and Post Conidtions
    // pre: x=x0 > 0 and y = y0 > 0
    z = x;
    while (2 <= y) do {
      z = z * x;
      y = y - 1;
    }
    // post: z = x0 ^ y0

    Trace the loop for some particularvalues:

         0     1     2     3
        ---------------------
    x  | 2     2     2     2
        ---------------------
    y  | 4     3     2     1
        ---------------------
    z  | 2     4     8     16
        ---------------------

    Determine a loop invariant:
      z . x ^ (y-1) = x0 ^ y0 and y >= 1
      z = x^(y0 - y + 1) and y >= 1 and x = x0

-}
