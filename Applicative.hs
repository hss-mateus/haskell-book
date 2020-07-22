module Applicative where

import Control.Applicative (liftA3)

-- Identity

newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a

-- Constant

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) _ (Constant a) = Constant a

-- Person

data Person =
  Person Name Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  Person <$> mkName n <*> mkAddress a

newtype Name =
  Name String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s =
  Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a =
  Address <$> validateLength 100 a

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if length s > maxLen
  then Nothing
  else Just s


-- Cow

data Cow = Cow
  { name :: String
  , age :: Int
  , weight :: Int
  }
  deriving (Eq, Show)

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
  liftA3 Cow (noEmpty name')
             (noNegative age')
             (noNegative weight')

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

-- List

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ append xs ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) (Cons x xs) = append fmapF fmapFs
    where fmapF = f <$> Cons x xs
          fmapFs = fs <*> Cons x xs

-- ZipList

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ pure x
  (<*>) (ZipList' Nil) _ = ZipList' Nil
  (<*>) _ (ZipList' Nil) = ZipList' Nil
  (<*>) (ZipList' (Cons f fs)) (ZipList' (Cons x xs)) = ZipList' $ append appHead appTail
    where appHead = Cons (f x) Nil
          (ZipList' appTail) = ZipList' fs <*> ZipList' xs

-- Validation

data Validation e a = Failure e
                    | Success a
                    deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) (Failure e) (Failure f) = Failure $ e <> f
  (<*>) (Failure e) _ = Failure e
  (<*>) _ (Failure e) = Failure e
  (<*>) (Success a) (Success b) = Success $ a b

-- Chapter exercises

data Pair a =
  Pair a a
  deriving Show

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)

data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two x f) (Two a b) = Two (x <> a) (f b)

data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three x y f) (Three a b c) = Three (x <> a) (y <> b) (f c)

data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' x f g) (Three' a b c) = Three' (x <> a) (f b) (g c)

data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four x y z f) (Four a b c d) = Four (x <> a) (y <> b) (z <> c) (f d)

data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Monoid a) => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (<*>) (Four' x y z f) (Four' a b c d) = Four' (x <> a) (y <> b) (z <> c) (f d)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
