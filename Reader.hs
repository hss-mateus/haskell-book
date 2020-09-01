module Reader where

import Data.Maybe (fromMaybe)

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (f . ra)

instance Applicative (Reader r) where
  pure = Reader . const
  (<*>) (Reader rab) (Reader ra) = Reader $ \r -> let a = ra r
                                                      rb = rab r a
                                                  in rb

instance Monad (Reader r) where
  (>>=) (Reader ra) arb = Reader $ \r -> let a = ra r
                                             rb = arb a
                                         in runReader rb r

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks = Reader

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f fa fb = f <$> fa <*> fb

newtype HumanName = HumanName String

newtype DogName = DogName String

newtype Address = Address String

data Person = Person { humanName :: HumanName
                     , dogName   :: DogName
                     , address   :: Address
                     }

data Dog = Dog { dogsName    :: DogName
               , dogsAddress :: Address
               }

getDog :: Person -> Dog
getDog = liftA2 Dog dogName address

-- Chapter exercises

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = do
  xs' <- xs
  ys' <- ys
  return (xs', ys')

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = liftA2 (,) z' z'

summed :: Num a => (a, a) -> a
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

sequA :: Integer -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ all id $ sequA 3
  print $ sequA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys
