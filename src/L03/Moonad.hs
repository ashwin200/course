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
--bind :: (a -> m b) -> m a -> m b
-- m a -> m [a]
-- lift2 :: (head -> tail -> head : tail) -> m a -> m b -> m c
-- mh :: m a
-- mt :: [m a]
-- lift2 (:) :: m a -> m [a] -> m [a]
seequence [] = reeturn []
seequence (mh:mt) = lift2 (:) mh (seequence mt)

seequence' :: Moonad m => [m a] -> m [a]
seequence' = foldr (lift2 (:)) (reeturn [])

-- Exercise 16
-- Relative Difficulty: 3
traaverse :: Moonad m => (a -> m b) -> [a] -> m [b]
--bind :: (a -> m b) -> m a -> m b
--seequence :: [m a] -> m [a]
--f :: a -> m b
--h :: a
--t :: [a]
--map f list :: (a->mb) -> [a] -> [mb]
traaverse f x = seequence  (map f x)

-- Exercise 17
-- Relative Difficulty: 4
reeplicate :: Moonad m => Int -> m a -> m [a]
--seequence :: [m a] -> m [a]
--reeplicate n ma = seequence (reeplicate' n ma [])
reeplicate n ma = seequence (repl n ma)

repl :: Int -> a -> [a]
repl n a 
   | n <= 0 = []
   | otherwise = a : repl (n-1) a

-- Exercise 18
-- Relative Difficulty: 9
filtering  :: Moonad m => (a -> m Bool) -> [a] -> m [a]
-- h :: a
-- f h :: m Bool
-- \ :: Bool -> a -> [a]
-- lift2 \ (f h) (m h) :: (Bool -> a -> [a]) -> m Bool -> m a -> m [a]
-- lift2 :: Moonad m => (a -> b -> c) -> m a -> m b -> m c 
-- lift2 (\b e -> if b then [e] else []) (f h) (reeturn h) :: m [a]
-- filtering f t :: m [a]
-- lift2 (++) m[a] m[a] :: ([a] -> [a] -> [a]) -> m[a] -> m[a] -> m[a]
filtering _ [] = reeturn []
filtering f (h:t) = lift2 (++) (lift2 (\b e -> if b then [e] else []) (f h) (reeturn h)) (filtering f t)

filtering' :: Moonad m => (a -> m Bool) -> [a] -> m [a]
filtering' f list = foldr (lift2 (++)) (reeturn []) (map (\h -> lift2 (\b e -> if b then [e] else []) (f h) (reeturn h)) list)


filtering'' :: Moonad m => (a -> m Bool) -> [a] -> m[a]
--bind :: (b -> m a) -> m b -> m a
--filtering'' f t -> m[a]
-- f h :: m Bool
-- fmaap' (\list -> h : list) (filtering'' f t) :: ([a] -> [a]) m [a]
filtering'' f list
   | null list = reeturn []
   | otherwise  = bind (\b -> if b then fmaap' (\list -> h : list) remaining else remaining) (f h) 
        where h = head(list)
              t = tail(list)
              remaining = filtering'' f t

            

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Moonad [] where
  bind = concatMap
  reeturn = return
