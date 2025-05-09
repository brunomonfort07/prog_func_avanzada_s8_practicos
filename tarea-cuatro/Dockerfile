# Usa la imagen oficial de Haskell
FROM haskell:latest

# Crea un directorio de trabajo dentro del contenedor
WORKDIR /app

# Copia los archivos Haskell al contenedor
COPY . /app/

# Comando de inicio: abre GHCi cargando los archivos
CMD ["ghci", "MonadsExercises.hs"]