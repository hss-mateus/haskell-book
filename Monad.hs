module Monad where

-- Nope

data Nope a = NopeDotJpg

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  (>>=) _ _ = NopeDotJpg

-- PhhhbbtttEither

data PhhhbbtttEither b a = Left' a | Right' b

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right' x) = Right' x
  fmap f (Left' x)  = Left' (f x)

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  (<*>) (Right' x) _        = Right' x
  (<*>)  _ (Right' x)       = Right' x
  (<*>) (Left' f) (Left' x) = Left' (f x)

instance Monad (PhhhbbtttEither b) where
  (>>=) (Left' x) f  = f x
  (>>=) (Right' x) _ = Right' x

-- Identity

newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

instance Monad Identity where
  (>>=) (Identity x) f = f x

-- List

data List a = Nil
            | Cons a (List a)
            deriving (Show)

instance Semigroup (List a) where
  (<>) Nil xs = xs
  (<>) (Cons x xs) ys = Cons x (xs <> ys)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) xs = (f <$> xs) <> (fs <*> xs)

instance Monad List where
  return = pure
  (>>=) Nil _         = Nil
  (>>=) (Cons x xs) f = f x <> (xs >>= f)

-- Functions

j :: Monad m => m (m a) -> m a
j ma = ma >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = f <$> m

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = f <$> ma <*> mb

a :: Monad m => m a -> m (a -> b) -> m b
a m mf = mf <*> m

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = (++) <$> ((: []) <$> f x) <*> meh xs f

flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs (j . return)
