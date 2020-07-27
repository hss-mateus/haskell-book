module Traversable where

-- Identity

newtype Identity a =
  Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

-- Constant

newtype Constant a b =
  Constant { getConstant :: a }

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldr _ b (Constant _) = b

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a

-- Optional

data Optional a = Nada
                | Yep a


instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

-- List

data List a = Nil
            | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

-- Three

data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

-- Pair

data Pair a b = Pair a b

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b


-- Big

data Big a b = Big a b b

instance Functor (Big a) where
  fmap f (Big a b c) = Big a (f b) (f c)

instance Foldable (Big a) where
  foldMap f (Big _ b c) = f b <> f c

instance Traversable (Big a) where
  traverse f (Big a b c) = Big a <$> f b <*> f c

-- Bigger

data Bigger a b = Bigger a b b b

instance Functor (Bigger a) where
  fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)

instance Foldable (Bigger a) where
  foldMap f (Bigger _ b c d) = f b <> f c <> f d

instance Traversable (Bigger a) where
  traverse f (Bigger a b c d) = Bigger a <$> f b <*> f c <*> f d

-- S

data S n a = S (n a) a

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (fmap f n) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S n a) = foldMap f n <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a

-- Tree

data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node x y z) = Node (fmap f x) (f y) (fmap f z)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node x y z) = foldMap f x <> f y <> foldMap f z

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node x y z) = Node <$> traverse f x <*> f y <*> traverse f z
