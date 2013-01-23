-- + Complete the 10 exercises below by filling out the function bodies.
--   Replace the function bodies (error "todo") with an appropriate solution.
-- + These exercises may be done in any order, however:
--   Exercises are generally increasing in difficulty, though some people may find later exercise easier.
-- + Bonus for using the provided functions or for using one exercise solution to help solve another.
-- + Approach with your best available intuition; just dive in and do what you can!

-- TOTAL marks:    /66

module L02.List where

-- BEGIN Helper functions and data types

-- The custom list type
data List t = Nil | t :| List t deriving Eq

-- Right-associative
infixr 5 :|

instance (Show t) => Show (List t) where
  show = show . foldRight (:) []

-- functions over List that you may consider using
foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil      = b
foldRight f b (h :| t) = f h (foldRight f b t)

foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ b Nil      = b
foldLeft f b (h :| t) = let b' = f b h in b' `seq` foldLeft f b' t

-- END Helper functions and data types

-- BEGIN Exercises

-- Exercise 1
-- Relative Difficulty: 1
-- Correctness: 2.0 marks
-- Performance: 0.5 mark
-- Elegance: 0.5 marks
-- Total: 3
headOr :: List a -> a -> a
headOr Nil = id
headOr (head :| tail) = \_ -> head

-- Exercise 2
-- Relative Difficulty: 2
-- Correctness:   2.5 marks
-- Performance: 1 mark
-- Elegance: 0.5 marks
-- Total: 4
suum :: List Int -> Int
suum list = foldLeft (+) 0 list

-- Exercise 3
-- Relative Difficulty: 2
-- Correctness: 2.5 marks
-- Performance: 1 mark
-- Elegance: 0.5 marks
-- Total: 4
len :: List a -> Int
len Nil = 0
len (_ :| tail) = 1 + len tail  
--len = foldLeft (\n _ -> n+1) 0
--len = foldLeft (const . succ) 0

-- Exercise 4
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.0 mark
-- Elegance: 1.5 marks
-- Total: 7
maap :: (a -> b) -> List a -> List b
maap _ Nil = Nil
maap f (head :| tail) = f(head) :| (maap f tail) 

-- Exercise 5
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.5 marks
-- Elegance: 1 mark
-- Total: 7
fiilter :: (a -> Bool) -> List a -> List a
--fiilter _ Nil = Nil
--fiilter f (head :| tail) = (if f(head) then (head :|) else id) (filter f tail)
fiilter p = foldRight (\h -> if p h then (h :|) else id) Nil

-- Exercise 6
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.5 marks
-- Elegance: 1 mark
-- Total: 7
append :: List a -> List a -> List a
append a b = foldRight (:|) b a
--append x y = flip (foldRight (:|)) x y
--append = flip (foldRight (:|))

-- Exercise 7
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.5 marks
-- Elegance: 1 mark
-- Total: 7
flatten :: List (List a) -> List a
--flatten Nil = Nil
--flatten (h :| t) = append h (flatten t) 
flatten = foldRight append Nil

-- Exercise 8
-- Relative Difficulty: 7
-- Correctness: 5.0 marks
-- Performance: 1.5 marks
-- Elegance: 1.5 mark
-- Total: 8
flatMap :: (a -> List b) -> List a -> List b
flatMap f = flatten . maap f

-- Exercise 9
-- Relative Difficulty: 8
-- Correctness: 3.5 marks
-- Performance: 2.0 marks
-- Elegance: 3.5 marks
-- Total: 9
seqf :: List (a -> b) -> a -> List b
seqf Nil _ = Nil
seqf (f :| t) a = f a :| seqf t a


sum3 :: String -> String -> String -> String
sum3 a b c = a ++ b ++ c

-- Exercise 10
-- Relative Difficulty: 10
-- Correctness: 5.0 marks
-- Performance: 2.5 marks
-- Elegance: 2.5 marks
-- Total: 10
rev :: List a -> List a
--rev list = rev0 list Nil
rev = foldLeft (\accum elem -> elem :| accum) Nil

rev0 :: List a -> List a -> List a
rev0 Nil b = b
rev0 (head :| tail) b = rev0 tail (head :| b)

-- END Exercises
