module Foldable where

import Data.Monoid

-- 1

sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

-- 2

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

-- 3

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = getAny . foldMap (Any . (== x))

-- 4

newtype Min a = Min { getMin :: Maybe a }

instance Ord a => Semigroup (Min a) where
  (<>) (Min Nothing) x = x
  (<>) x (Min Nothing) = x
  (<>) (Min x) (Min y) = Min $ min <$> x <*> y

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = getMin . foldMap (Min . Just)

-- 5

newtype Max a = Max { getMax :: Maybe a }

instance Ord a => Semigroup (Max a) where
  (<>) (Max Nothing) x = x
  (<>) x (Max Nothing) = x
  (<>) (Max x) (Max y) = Max $ max <$> x <*> y

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = getMax . foldMap (Max . Just)

-- 6

null :: Foldable t => t a -> Bool
null = getAll . foldMap (const $ All False)

-- 7

length :: Foldable t => t a -> Int
length = getSum . foldMap (const $ Sum 1)

-- 8

toList :: Foldable t => t a -> [a]
toList = foldMap (: [])

-- 9

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

-- 10

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty

-- 11

newtype Constant a b = Constant b

instance Foldable (Constant a) where
  foldMap f (Constant x) = f x

-- 12

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

-- 13

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

-- 14

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ b c) = f b <> f c

-- 15

data Four a b = Four a b b b

instance Foldable (Four a) where
  foldMap f (Four _ b c d) = foldMap f [b, c, d]

-- 16

filterF :: (Applicative f
           , Foldable t
           , Monoid (f a))
        => (a -> Bool)
        -> t a
        -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
