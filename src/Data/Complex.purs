module Data.Complex where

import Prelude

data Complex a = Complex a a

derive instance Eq a => Eq (Complex a)

instance Show a => Show (Complex a) where
  show (Complex x y) = "Complex " <> show x <> " " <> show y

instance Ring a => Semiring (Complex a) where
  add (Complex x y) (Complex z w) = Complex (x + z) (y + w)
  zero = Complex zero zero
  mul (Complex x y) (Complex z w) = Complex (x * z - y * w) (x * w + y * z)
  one = Complex one zero

instance Ring a => Ring (Complex a) where
  sub (Complex x y) (Complex z w) = Complex (x - z) (y - w)

instance CommutativeRing a => CommutativeRing (Complex a)

instance DivisionRing a => DivisionRing (Complex a) where
  recip (Complex x y) = (Complex (x * recip d) (-y * recip d))
    where
      d = (x * x + y * y)

instance EuclideanRing (Complex Number) where
  degree _ = 1
  div z w = z * recip w
  mod _ _ = zero

real :: forall a. Complex a -> a
real (Complex x _) = x

imag :: forall a. Complex a -> a
imag (Complex _ y) = y

i :: forall a. Semiring a => Complex a
i = Complex zero one

magnitude :: forall a. Semiring a => Complex a -> a
magnitude (Complex x y) = x * x + y * y