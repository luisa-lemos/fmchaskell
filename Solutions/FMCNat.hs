{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where

    O == O = True
    (S n) == (S m) = n == m
    _ == _  = False

instance Ord Nat where

    O <= _ = True
    (S _) <= O = False
    (S n) <= (S m) = n <= m

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min O _ = O
    min _ O = O
    min (S n) (S m) = S (min n m)

    max O n = n
    max n O = n
    max (S n) (S m) = S (max n m)


----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

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

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero _ = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S O) = False
even (S (S n)) = even n

odd :: Nat -> Bool
odd O     = False
odd (S n) = even n


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
O     <+> n = n
(S m) <+> n = S (m <+> n)


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
times :: Nat -> Nat -> Nat
times O _ = O
times (S m) n = n <+> times m n

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow _ O   = one
pow m (S n) = times m (pow m n)

exp :: Nat -> Nat -> Nat
exp _ O  = one
exp m (S n) = pow m (exp m n)

(<^>) :: Nat -> Nat -> Nat
_ <^> O  = one
m <^> (S n) = exp m (m <^> n)


-- monus (helper function) ((m <-> n) ou é m - n, ou O se n é maior que m.)
(<->) :: Nat -> Nat -> Nat
m  <-> O  = m
O <-> _  = O
(S m) <-> (S n) = m <-> n

-- quotient
(</>) :: Nat -> Nat -> Nat
_ </> O = error "Division by zero"
m </> n | n > m = O
        | otherwise    = S ((m <-> n) </> n)

-- remainder
(<%>) :: Nat -> Nat -> Nat
_ <%> O = error "Division by zero"
m <%> n | n > m = m
        | otherwise    = (m <-> n) <%> n


-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (m, n) = (m </> n, m <%> n)

-- divides
(<|>) :: Nat -> Nat -> Bool
O <|> O = True
O <|> _ = False
(S n) <|> m = m <%> S n == O

divides = (<|>)


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist x y | y <= x    = x <-> y
         | otherwise = y <-> x

(|-|) = dist

factorial :: Nat -> Nat
factorial O     = one
factorial (S m) = times (S m) (factorial m)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O     = O
sg (S _) = one

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O     _ = error "base não pode ser zero"
lo (S O) _ = error "base não pode ser um"
lo _     O = error "log de zero é indefinido"
lo b     x | b > x = O
           | otherwise    = S (lo b (x </> b))


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat n | n < 0 = error "toNat: input negativo"
        | n == 0 = O
        | otherwise = S (toNat (n - 1))

fromNat :: Integral a => Nat -> a
fromNat O  = 0
fromNat (S n) = 1 + fromNat n


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0  = error "Não se pode representar inteiros negativos como nats."
      | x == 0  = O
      | otherwise = S (fromInteger (x - 1))

