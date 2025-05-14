-- Denise Souberville 223427
-- Bruno Monfort 173280
-- Nicolás Capellino 272778

module ParserExercises where

import Parsing


{- PRIMER EJERCICIO:
Dar un parser para strings 
"{ field1: bool, field2: int, field3: string }" 
el resultado debera ser una lista de tuplas con el nombre del field y el tipo asociado [(String,String)], un bnf del lenguaje es el siguiente:
record ::= { identifier : type ( , field : type )* }
type ::= bool | int | string
-}

-- +++ sale de Parsing.hs es lo mismo que <|> del libro. Osea intenta aplicar el parser de la izquierda.. si falla, entonces intenta con el parser de la derecha.
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
    aType <- symbol "bool" +++ symbol "int" +++ symbol "string"
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


-- codigo del libro... TO DO: AGREGAR LA RESTA
expr :: Parser Int
expr = do t <- term
          do
            symbol "+"
            e <- expr
            return (t + e)
          +++
          return t

-- codigo del libro... TO DO: AGREGAR LA DIVISIÓN
term :: Parser Int
term = do f <- factor
          do 
            symbol "*"
            t <- term
            return (f * t)
          +++ return f

-- codigo del libro... TO DO: CAMBIAR A INTEGER
factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         +++ natural