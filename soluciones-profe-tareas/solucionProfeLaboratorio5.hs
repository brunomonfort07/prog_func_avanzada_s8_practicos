module Laboratorio5 where

import Parsing
import Control.Applicative ((<|>))

type Field = (String, String) -- (Identifier, Field Type)

type Record = [Field]

pType :: Parser String -- Un tipo es una de las tres cadenas de caracteres
pType = symbol "bool" <|> symbol "int" <|> symbol "string"

pIdentifier :: Parser String -- Un identificador es una cadena de caracteres que empieza con una letra y puede tener letras o números
pIdentifier = identifier

pField :: Parser Field -- Un campo es un identificador seguido de dos puntos y un tipo, para ello utilizo el parser de identificador y el parser de tipo
pField = do
    identifier <- pIdentifier
    symbol ":"
    typeN <- pType
    return (identifier, typeN)

pFieldList :: Parser [Field] -- Obtengo el primer campo obligatorio y luego los opcionales (sacando las comas)
pFieldList = do
    field <- pField
    fields <- many (do symbol ","
                       pField)
    return (field : fields)

pRecord :: Parser Record -- Del texto que quiero parsear, le saco las llaves
pRecord = do
    symbol "{"
    fieldList <- pFieldList
    symbol "}"
    return fieldList

parseRecord :: String -> Record
parseRecord input = case parse (do space; r <- pRecord; space; return r) input of
    [(record, [])] -> record
    _              -> error "parse error"


-- Ej 6 Hutton
factor :: Parser Int -- Factor esun num o algo entre parentesis
factor = integer
     <|> do symbol "("
            e <- expr
            symbol ")"
            return e

term :: Parser Int -- Term es para multiplicar y dividir con prioridad
term = do
  f <- factor
  do symbol "*"
     t <- term
     return (f * t)
   <|> do symbol "/"
          t <- term
          return (f `div` t)
   <|> return f

expr :: Parser Int -- Expr para tomar en cuenta suma y resta
expr = do
  t <- term
  do symbol "+"
     e <- expr
     return (t + e)
   <|> do symbol "-"
          e <- expr
          return (t - e)
   <|> return t
