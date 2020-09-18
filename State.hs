{-# LANGUAGE TupleSections #-}

module State where

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State g) = State $ \s -> let (a, s') = g s
                                   in (f a, s')

instance Applicative (State s) where
  pure a = State (a,)
  (<*>) (State f) (State g) = State $ \s -> let (f', s') = f s
                                                (a, s'') = g s'
                                            in (f' a, s'')

instance Monad (State s) where
  return = pure
  (>>=) (State f) g = State $ \s -> let (a, s') = f s
                                        (State h) = g a
                                    in h s'

state :: (s -> (a, s)) -> State s a
state = State

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

exec :: State s a -> s -> s
exec (State sa) = snd . sa

eval :: State s a -> s -> a
eval (State sa) = fst . sa

modify :: (s -> s) -> State s ()
modify f = State $ ((),) . f
