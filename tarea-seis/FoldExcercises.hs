-- Denise Souberville 223427
-- Bruno Monfort 173280
-- Nicolás Capellino 272778

module FoldExercises where
import Data.Foldable

{- PRIMER EJERCICIO:
4. In a similar manner, show how the following type of binary trees with data
in their nodes can be made into a foldable and traversable type:
data Tree a = Leaf | Node (Tree a)  (Tree a)
deriving Show
-}

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show
    
instance Foldable Tree where
  fold :: Monoid m => Tree m -> m
  fold Leaf = mempty
  -- Plegar los subárboles izquierdo y derecho, e incluir el nodo a en el resultado.
  fold (Node l a r) = fold l `mappend` a `mappend` fold r 

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f Leaf = mempty 
  -- Plegar los subárboles izquierdo y derecho, e incluir el nodo a en el resultado.
  foldMap f (Node l a r) = foldMap f l `mappend` f a `mappend` foldMap f r

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f b Leaf = b
  foldr f b (Node l a r) = foldr f (f a (foldr f b r)) l

{-foldr - Qué está pasando:
Se hace el foldr del subárbol derecho con b, obteniendo rightResult.
Se aplica f al valor actual a y rightResult, dando midResult.
Se hace el foldr del subárbol izquierdo con midResult.

→ Esto es exactamente el comportamiento esperado de foldr:
recorrer de derecha a izquierda e ir aplicando f.

Objetivo: Procesar una estructura desde el final hacia el principio, manteniendo 
la posibilidad de trabajar con estructuras infinitas (en algunos casos) y permitir 
operaciones de corte temprano (como take o short-circuiting).
-}

  foldl :: (b -> a -> b) -> b -> Tree a -> b
  foldl f b Leaf = b
  foldl f b (Node l a r) = foldl f (f (foldl f b l) a) r 

{-foldl - 
Primero aplica foldl al subárbol izquierdo con el acumulador b, obteniendo b'.
Luego aplica f b' a, usando el valor del nodo actual, obteniendo b''.
Finalmente, aplica foldl al subárbol derecho con b''.

Objetivo: Procesar la estructura desde el principio hacia el final, acumulando resultados 
paso a paso. Se asocia a la izquierda y es útil cuando querés acumular un resultado de 
forma estricta (como contar, sumar, etc.).
-}


{- En el libro hay un Functor Tree con el a en la Leaf -}
instance Functor Tree where
  fmap f Leaf = Leaf
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

{-traverse = Aplica una función que devuelve un valor con efectos a
  cada elemento de la estructura.
  Devuelve una estructura dentro del contexto del efecto.

Ejemplos traverse:
traverse Just [1,2,3]   -- Resultado: Just [1,2,3]
traverse (\x -> if x > 0 then Just x else Nothing) [1,2,3] -- Just [1,2,3]
traverse (\x -> if x > 0 then Just x else Nothing) [1,0,3] -- Nothing
traverse Just (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) 
    -- Resultado: Just (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf))

  Objetivo pure: tomar un valor normal y envolverlo en un contexto.
    Recibe un valor de tipo a.
    Lo eleva o inyecta en un contexto f (por ejemplo, Maybe, [], IO, Tree, etc).
    No realiza ningún efecto adicional. Solo "empaqueta" el valor.-}

--instance Traversable Tree where
     -- traverse :: Applicative f =>
     -- (a -> f b)-> Tree a -> f (Tree b)
     -- traverse g (Leaf x) = pure Leaf <*> g x
     --traverse g (Node l r) = pure Node <*> traverse g l <*> traverse g r

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g Leaf = pure Leaf
  traverse g (Node l a r) = pure Node <*> traverse g l <*> g a <*> traverse g r

{- SEGUNDO EJERCICIO:
5. Using foldMap, define a generic version of the higher-order function filter
on lists that can be used with any foldable type:
-}
--DUDA: Esta bien el cambio de firma? (se agregó "Monoid a")
filterF :: (Foldable t, Monoid a) => (a -> Bool) -> t a -> [a]
filterF p ta = case p (fold ta) of {
    True -> undefined;
    False -> undefined
}

{-instance Foldable t where
  fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
        foldMap :: Monoid [a] => (a -> [a]) -> t a -> [a]
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldl :: (b -> a -> b) -> b -> Tree a -> b
  -}