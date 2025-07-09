module Laboratorio7 where


--15.9 Exercises 227
--5. Define appropriate versions of the library functions
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

repeatTree :: a -> Tree a
repeatTree x = Node (repeatTree x) x (repeatTree x)

takeTree :: Int -> Tree a -> Tree a
takeTree 0 _    = Leaf
takeTree _ Leaf = Leaf
takeTree n (Node l x r) = Node (takeTree m2 l) x (takeTree (m - m2) r)
  where m = n - 1
        m2 = m `div` 2

replicateTree :: Int -> a -> Tree a
replicateTree n = takeTree n . repeatTree 

-- 6.

infaprox :: Double -> [Double]
infaprox n = 1.0 : [ (next + n/next)/2 | next <- infaprox n ]

sqroot :: Double -> Double
sqroot n = fst $ head $ dropWhile (\ (next, prev) -> abs (next - prev) > 0.00001)  $ zip (tail (infaprox n)) (infaprox n)
