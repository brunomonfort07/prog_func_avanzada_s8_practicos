-- Denise Souberville 223427
-- Bruno Monfort 173280
-- Nicolás Capellino 272778

module ParserExercises where

import Parsing
import Control.Applicative (Alternative((<|>)))


{- PRIMER EJERCICIO:
Dar un parser para strings 
"{ field1: bool, field2: int, field3: string }" 
el resultado debera ser una lista de tuplas con el nombre del field y el tipo asociado [(String,String)], un bnf del lenguaje es el siguiente:
record ::= { identifier : type ( , field : type )* }
type ::= bool | int | string
-}

-- symbol sale de Parsing.hs sirve para reconocer una cadena de texto determinada ignorando los espacios en blanco alrededor. Es una versión "espaciada" del parser string.


-- oneField  toma un campo como 'field1: bool' y devuelve '("field1", "bool")'.
-- Este parser:
--      Espera un nombre de campo (identifier)
--      Luego el símbolo :
--      Luego uno de los tipos permitidos (bool, int, string)
--      Devuelve una tupla con el nombre del campo y el tipo
oneField :: Parser (String, String)
oneField = do
    fieldName <- identifier
    _ <- symbol ":"
    aType <- symbol "bool" <|> symbol "int" <|> symbol "string"
    return (fieldName, aType)

-- fields extiende el uso de oneField a varios fields y los separa por coma
-- Este parser:
--      Espera al menos un oneField
--      Luego cero o más campos adicionales precedidos por ,
--      Junta todos los campos en una lista
fields :: Parser [(String, String)]
fields = do
    f  <- oneField
    fs <- many (do _ <- symbol ","; oneField)         
    return (f : fs)

-- record recibe el string entre llaves y devuelve la lista de tuplas con el nombre del field y el tipo asociado [(String,String)]
-- Este parser:
--      Espera una llave izquierda {
--      Luego los campos
--      Luego una llave derecha }
--      Devuelve la lista de campos
record :: Parser [(String, String)]
record = do
    _ <- symbol "{"
    fs <- fields
    _ <- symbol "}"
    return fs

{- SEGUNDO EJERCICIO:
Ejercicio 6 del Hutton (pag. 194)
Extend the parser expr :: Parser Int to support subtraction and division,
and to use integer values rather than natural numbers, based upon the following
revisions to the grammar:
expr   ::= term (+ expr | - expr | ε)
term   ::= factor (* term | / term | ε)
factor ::= ( expr ) | int
int    ::= ..., -1, 0, 1, ...
-}

expr :: Parser Int
expr = do
  t <- term
  do symbol "+"
     e1 <- expr
     return (t + e1)
     <|> do symbol "-" 
            e2 <- expr
            return (t - e2)
     <|> return t

term :: Parser Int
term = do 
    f <- factor
    do symbol "*"
       t1 <- term
       return (f * t1)
       <|> do symbol "/"
              t2 <- term
              if t2 == 0
              then failure
              else return (f `div` t2)
       <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         <|> integer


parseTest4 = parse expr "2 + 3 * 4"
-- Esperado: [(14,"")]

-- Resta
parseTest6 = parse expr "10 - 2"
-- Esperado: [(8,"")]

-- División con números enteros
parseTest7 = parse expr "20 / 5"
-- Esperado: [(4,"")]

-- División por cero
parseTest8 = parse expr "8 / 0"
-- Esperado: [] (falla silenciosamente)

-- Expresión compleja con múltiples operaciones
parseTest9 = parse expr "1 + 2 * (3 + 4) - 5"
-- Esperado: [(10,"")]

-- Negativos
parseTest10 = parse expr "-3 + 7"
-- Esperado: [(4,"")]

-- Expresión compleja con múltiples operaciones
parseTest11 = parse expr "1 + 2 * (2 + 6) / 4"