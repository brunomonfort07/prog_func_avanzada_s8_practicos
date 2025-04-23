-- Denise Souberville 223427
-- Bruno Monfort 173280
-- Nicolás Capellino 272778

-- Cap 17 Applicative

module ApplicativesExercises where

--
--(Pag 692) Implementar Functor y Applicative para Validation

data Validation err a = Failure err | Success a 
     deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    (Failure x) <> (Failure y) = Failure (x <> y)
    (Success x) <> (Success y) = Success x
    Failure x <> _ = Failure x
    _ <> Failure y = Failure y
    
instance Functor (Validation e) where
     fmap _ (Failure err) = Failure err
     fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
     pure = Success
     Failure a <*> Failure b = Failure (a <> b)
     Failure err <*> _ = Failure err
     _ <*> Failure err = Failure err
     Success fA <*> Success a = Success (fA a)

--
-- Implementar Applicative para Seccion 17.9 (pag 694) Ej 4. data Three a b c = Three a b c
data Three a b c = Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid e, Monoid f) => Applicative (Three e f) where
    pure a = Three mempty mempty a
    Three tA1 tB1 fA <*> Three tA2 tB2 a = Three (tA1 <> tA2) (tB1 <> tB2) (fA a)