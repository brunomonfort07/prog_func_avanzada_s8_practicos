# TAREA DOS


## Ejecutar sin Docker

### Comandos para ejecutar los archivos .hs, por ejemplo, cargar el archivo A.hs :

<code>
cd .\tarea-uno\
<br/> 
ghci .\a.hs
</code>

## Ejecutar con Docker

<code>
docker build -t haskell-ghci .
<br/> 
docker run -it --rm haskell-ghci
</code>
