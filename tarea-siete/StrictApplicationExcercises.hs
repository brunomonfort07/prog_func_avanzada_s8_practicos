-- Denise Souberville 223427
-- Bruno Monfort 173280
-- Nicolás Capellino 272778

-- Cap 15 Hutton. Strict Applicatives. 
module StrictApplicationExercises where

{- SEGUNDO EJERCICIO:
5. Define appropriate versions of the library functions
repeat :: a -> [a]
repeat x = xs where xs = x:xs

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs

replicate :: Int -> a -> [a]
replicate n = take n . repeat

for the following type of binary trees:
data Tree a = Leaf | Node (Tree a) a (Tree a)
deriving Show
-}

data Tree a = Leaf | Node (Tree a) a (Tree a)
deriving Show

repeatTree :: a -> Tree a
repeatTree x = xs where xs = Node xs x xs

takeTree :: Int -> Tree a -> Tree a
takeTree 0 _ = Leaf
takeTree _ Leaf = Leaf
takeTree n (Node l x r) = Node (takeTree (n-1) l) x (takeTree (n-1) r)


replicate :: Int -> a -> Tree a
replicateTree n =  --TO DO

{- SEGUNDO EJERCICIO:
6. Newton’s method for computing the square root of a (non-negative) floating-point
number n can be expressed as follows:
- start with an initial approximation to the result;
- given the current approximation a, the next approximation is defined by
the function next a = (a + n/a) / 2;
- repeat the second step until the two most recent approximations are within
some desired distance of one another, at which point the most recent
value is returned as the result.

Define a function sqroot :: Double -> Double that implements this procedure.
Hint: first produce an infinite list of approximations using the library
function iterate. For simplicity, take the number 1.0 as the initial approximation,
and 0.00001 as the distance value.
-}


-- iterate f x = x : iterate f (f x)
-- iterate (+1) 0
-- [0,1,2,3,4,5,6,7,8,9,...]  (lista infinita de enteros crecientes)

sqroot :: Double -> Double
sqroot x = -- TO DO