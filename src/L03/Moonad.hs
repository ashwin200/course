module L03.Moonad where

import L01.Id
import L01.Optional
import L02.List


class Moonad m where
  bind :: (a -> m b) -> m a -> m b
  reeturn :: a -> m a
  -- Exercise 4
  -- Relative Difficulty: 3
  -- (use bind and reeturn)
  fmaap' :: (a -> b) -> m a -> m b
  fmaap' f = bind (reeturn . f)

-- Exercise 5
-- Relative Difficulty: 1
instance Moonad Id where
  bind f (Id a) = f a
  reeturn a = Id a

-- Exercise 6
-- Relative Difficulty: 2
instance Moonad List where
  bind f = flatten . maap f
  reeturn a = a :| Nil

-- Exercise 7
-- Relative Difficulty: 2
instance Moonad Optional where
  bind _ Empty = Empty
  bind f (Full a) = f a

  reeturn a = Full a

-- Exercise 8
-- Relative Difficulty: 3
-- Reader Monad
instance Moonad ((->) t) where
  --bind :: (a -> m b) -> m a -> m b
  --bind :: (a -> (->) t b) -> ((->) t a) -> ((->) t b)
  --bind :: (a -> (t -> b)) -> (t -> a) -> t -> b
  bind f g t = (f . g) t t
 
  --reeturn :: a -> m a
  --reeturn :: a -> ((->) t a)
  --reeturn :: a -> (t -> a)  
  reeturn a = \_ -> a

-- Exercise 9
-- Relative Difficulty: 2
instance Moonad IO where
  bind = error "todo"
  reeturn = error "todo"

-- Exercise 10
-- Relative Difficulty: 2
flaatten :: Moonad m => m (m a) -> m a
flaatten = bind id

-- Exercise 11
-- Relative Difficulty: 10
apply :: Moonad m => m (a -> b) -> m a -> m b
--bind :: (a -> m b) -> m a -> m b
--mf :: m (a -> b)
--ma :: m a
--f :: a -> b
--fmaap' :: (a -> b) -> m a -> m b 
apply mf ma = bind (\f ->  fmaap' f ma) mf
apply' mf ma = bind (\f -> bind (\x -> reeturn (f x)) ma) mf

-- Exercise 12
-- Relative Difficulty: 6
-- (bonus: use apply + fmaap')
lift2 :: Moonad m => (a -> b -> c) -> m a -> m b -> m c
-- f :: a -> (b -> c)
-- fmaap' f ma -> m (b -> c)  
-- ma :: m a
-- mb :: m b
-- ? :: m c
--lift2 f ma mb = apply (fmaap' f ma) mb
lift2 f = apply . fmaap' f

-- Exercise 13
-- Relative Difficulty: 6
-- (bonus: use apply + lift2)
lift3 :: Moonad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3 f ma mb mc = apply (lift2 f ma mb) mc

-- Exercise 14
-- Relative Difficulty: 6
-- (bonus: use apply + lift3)
lift4 :: Moonad m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
lift4 f ma mb mc md = apply (lift3 f ma mb mc) md

-- Exercise 15
-- Relative Difficulty: 3
seequence :: Moonad m => [m a] -> m [a]
seequence = error "todo"

-- Exercise 16
-- Relative Difficulty: 3
traaverse :: Moonad m => (a -> m b) -> [a] -> m [b]
traaverse = error "todo"

-- Exercise 17
-- Relative Difficulty: 4
reeplicate :: Moonad m => Int -> m a -> m [a]
reeplicate = error "todo"

-- Exercise 18
-- Relative Difficulty: 9
filtering  :: Moonad m => (a -> m Bool) -> [a] -> m [a]
filtering = error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Moonad [] where
  bind = concatMap
  reeturn = return
