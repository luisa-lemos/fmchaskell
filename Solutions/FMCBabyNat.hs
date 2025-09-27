module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined )
import Distribution.SPDX (LicenseId(SHL_0_5))

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- some sugar
zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

-- addition
(+) :: Nat -> Nat -> Nat
n + O   = n
n + S m = S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero O = S O
isZero (S _) = O

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

-- Output: O means False, S O means True
even :: Nat -> Nat
even O = S O
even (S O) = O
even (S (S n)) = even n 

odd :: Nat -> Nat
odd O = O
odd (S O) = S O
odd (S (S n)) = odd n

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus m O = m
monus O _ = O
monus (S m) (S n) = monus m n


(-*) :: Nat -> Nat -> Nat
(-*) = monus

-- multiplication
(*) :: Nat -> Nat -> Nat
m * O     = O
m * (S n) = (m * n) + m

infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
m ^ O     = S O
m ^ (S n) = m * (m ^ n)

infixr 8 ^

-- quotient
--- primeiro, vou definir o "<":
---- Output: O means False, S O means True
(<) :: Nat -> Nat -> Nat
O   < (S _) = S O
_   < O     = O
(S m) < (S n) = m < n

infix 4 <

--- agora, posso definir quotient:
(/) :: Nat -> Nat -> Nat
m / O  = O
m / n = case m < n of
          S O -> O
          O   -> S ((m -* n) / n)

infixl 7 /

-- remainder
(%) :: Nat -> Nat -> Nat
m % O = O 
m % n = case m < n of
    S O -> m
    O -> (m -* n) % n

infixl 7 %

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
O   ||| _ = O
n   ||| m = isZero (m % n)

divides :: Nat -> Nat -> Nat
divides = (|||)

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff x y = case x < y of
                S O -> y -* x
                O   -> x -* y

(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff

factorial :: Nat -> Nat
factorial O     = one
factorial (S n) = S n * factorial n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg (S _) = one

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo b a = case b of
          O -> O
          (S O) -> O
          _    -> case a < b of
                       S O -> O
                       O   -> S (lo b (a / b))

