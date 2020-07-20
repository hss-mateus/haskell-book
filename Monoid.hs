{-# LANGUAGE TupleSections #-}

module Monoid where

import Semigroup

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance Monoid BoolConj where
  mempty = BoolConj True

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine mempty

instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp mempty

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance (Semigroup a) => Semigroup (Mem s a) where
  (<>) Mem { runMem = f } Mem { runMem = g } =
    Mem $ \x -> let (a, b) = g x
                    (c, d) = f b
                in (a <> c, d)

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (mempty,)
