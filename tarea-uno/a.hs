-- Denise Souberville 223427
-- Bruno Monfort 173280
-- Nicolás Capellino 272778

module A where
import Data.List (sortBy)

-- Problem description
--
-- Given a sequence of numbers and a target number, attempt to construct an expression whose:
-- - values are in the target, 
-- - the expression is formed by combining one or more numbers from the sequence using addition, subtraction, multiplication, division and parentheses.

data Op = Add | Sub | Mul | Div | Pot
   deriving (Eq)

instance Show Op where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"
   show Div = "/"
   show Pot = "^"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = y /= 0 && x `mod` y == 0
valid Pot x y = y >= 0 

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Pot x y = x ^ y

-- Aplicar currificacion en la anterior definicion?

data Expr = Val Int | App Op Expr Expr

-- Ejemplo1: Como representamos 1 + 2 * 3 ?

expresionExample :: Expr
expresionExample = App Add (Val 1) (App Mul (Val 2) (Val 3))

-- Ejemplo2: Como representamos 1 ^ 2 * 3 ?

expresionExampleOtro :: Expr
expresionExampleOtro = App Mul (App Pot (Val 1) (Val 2)) (Val 3) 

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
      where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

-- Lista para manejar errores en la evaluacion de expresiones, teniendo dos posibilidades
--     - la lista esta vacia, no hay solucion
--     - la lista tiene un solo unico valor que es el resultado de la evauacion de la expresion

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [ apply o x y |
                      x <- eval l,
                      y <- eval r,
                      valid o x y]

-- Combinaciones

-- subs returns all subsequences of a list, which are given by all possible combinations of excluding or including
-- each element of the list, 

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

-- interleave returns all possible ways of inserting a new element into a list

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- perms returns all permutations of a list, which are given by all possible reorderings of the elements

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- choices from a list, which are given by
-- all possible ways of selecting zero or more elements in any order, can then be
-- defined simply by considering all permutations of all subsequences

choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- Especificando una solucion al problema countdown

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- (1 + 50) * (25 - 10)

expresionExample2 :: Expr
expresionExample2 = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))

-- Mas eficiente

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice xs ys = all (`elem` ys) xs

solution2 :: Expr -> [Int] -> Int -> Bool
solution2 e ns n = isChoice (values e) ns && eval e == [n]

-- Solucion por fuerza bruta

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

-- exprs which returns all possible expressions whose list of values is precisely a given list

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
              l <- exprs ls,
              r <- exprs rs,
              e <- combine l r ]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add,Sub,Mul,Div,Pot]]

-- Solucion

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns,
                      e <- exprs ns',
                      eval e == [n]]

-- Combinando generacion y evaluacion

type Result = (Expr,Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls,rs) <- split ns,
                  lx <- results ls,
                  ry <- results rs,
                  res <- combine' lx ry ]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) =
  [(App o l r, apply o x y) | o <- [Add,Sub,Mul,Div,Pot], valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
  [e | ns' <- choices ns, (e,m) <- results ns', m == n]

-- Explotando propiedades algebraicas

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && y /= 0 && x `mod` y == 0
valid' Pot x y = y >= 0 && y /= 1 && x /= 1

-- Ver con el profe línea anterior
-- simplemente cambiando valid' por valid en la anterior solucion

combine'' :: Result -> Result -> [Result]
combine'' (l,x) (r,y) =
  [(App o l r, apply o x y) | o <- [Add,Sub,Mul,Div,Pot], valid' o x y]

results' :: [Int] -> [Result]
results' [] = []
results' [n] = [(Val n,n) | n > 0]
results' ns = [res | (ls,rs) <- split ns,
                  lx <- results' ls,
                  ry <- results' rs,
                  res <- combine'' lx ry ]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n =
  [e | ns' <- choices ns, (e,m) <- results' ns', m == n]