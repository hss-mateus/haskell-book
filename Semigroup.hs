module Semigroup where

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) Trivial Trivial = Trivial

newtype Identity a = Identity a

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity x) (Identity y) = Identity $ x <> y

data Two a b = Two a b

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two x y) (Two m n) = Two (x <> m) (y <> n)

data Three a b c = Three a b c

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (<>) (Three x y z) (Three m n o) = Three (x <> m) (y <> n) (z <> o)

data Four a b c d = Four a b c d

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (<>) (Four a b c d) (Four m n o p) = Four (a <> m) (b <> n) (c <> o) (d <> p)

newtype BoolConj = BoolConj Bool

instance Semigroup BoolConj where
  (<>) (BoolConj x) (BoolConj y) = BoolConj $ x && y

newtype BoolDisj = BoolDisj Bool

instance Semigroup BoolDisj where
  (<>) (BoolDisj x) (BoolDisj y) = BoolDisj $ x || y

data Or a b = Fst a
            | Snd b

instance Semigroup (Or a b) where
  (<>) (Snd x) _ = Snd x
  (<>) _ (Snd x) = Snd x
  (<>) _ (Fst x) = Fst x

newtype Combine a b = Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine (\x -> f x <> g x)

newtype Comp a = Comp { unComp :: a -> a }

instance Semigroup a => Semigroup (Comp a) where
  (<>) Comp { unComp = f } Comp { unComp = g } = Comp $ f <> g

data Validation a b = Failure a
                    | Success b
                    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Success x) _ = Success x
  (<>) _ (Success x) = Success x
  (<>) (Failure x) (Failure y) = Failure $ x <> y
