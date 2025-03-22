# Sin Docker

## Comandos para ejecutar los archivos .hs, por ejemplo, cargar el archivo A.hs:

<code>
cd .\tarea-uno\
<br/> 
ghci .\a.hs
</code>

# Con Docker

<code>
docker build -t haskell-ghci .
<br/> 
docker run -it --rm haskell-ghci
</code>

# Ejecutar pruebas sobre los módulos:

1. Cargar el archivo A.hs en el intérprete de Haskell.
2. Ejecutar las pruebas copiando y pegando las siguientes líneas en el intérprete:

    - Add
    - valid Sub 2 4
    - apply Add 2 3
    - expresionExample
    - eval expresionExample
    - eval (App Div (Val 1) (Val 0))
    - subs [1,2,3]
    - interleave 0 [1,2]
    - perms [1,2,3]
    - choices [1,2,3]
    - solution expresionExample2 [1,3,7,10,25,50] 765
    - isChoice [1,2,3] [1,2,3,4]
    - solution2 expresionExample2 [1,3,7,10,25,50] 765
    - split [1,2,3,4]
    - exprs [1,2]
    - solutions [1,9] 10
    - solutions' [1,3,7,10,25,50] 765
    - solutions' [1,3,7,10,25,50] 765