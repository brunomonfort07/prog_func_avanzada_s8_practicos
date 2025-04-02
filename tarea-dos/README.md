# TAREA DOS

## Ejercicio Semigrupos - EJERCICIO 9 - COMBINE

### Explicación

```haskell
instance Semigroup b => Semigroup (Combine a b) where
    (<>) (Combine f) (Combine g) = Combine $ \x -> f x <> g x
```

1. **`instance Semigroup b => Semigroup (Combine a b)`**
   
   - Aquí estamos diciendo que queremos definir la instancia de `Semigroup` para el tipo `Combine a b`.
   - La condición `Semigroup b` significa que el tipo `b` tiene que ser un `Semigroup`, es decir, tiene que tener definida la operación `(<>)` que permite combinar valores de tipo `b`.

2. **`(<>) (Combine f) (Combine g) = ...`**
   
   - Esta es la definición de la operación `(<>)` para dos valores de tipo `Combine a b`.
   - Aquí estamos desestructurando los dos valores de tipo `Combine a b`, llamándolos `Combine f` y `Combine g`. Esto significa que `f` y `g` son funciones de tipo `a -> b`, que es lo que guarda cada valor dentro de `Combine`.

3. **`Combine $ \x -> f x <> g x`**
   
   - Este es el cuerpo de la operación `(<>)`. Lo que hacemos es crear una nueva instancia de `Combine`.
   - La nueva instancia de `Combine` toma una función que recibe un valor `x` de tipo `a` y devuelve un valor de tipo `b` (como cualquier otra función dentro de `Combine`).
   - **¿Qué hace esta nueva función?**
     - La nueva función toma el valor `x`, lo aplica a `f` y a `g` (es decir, calcula `f x` y `g x`).
     - Luego, combina los resultados de `f x` y `g x` utilizando la operación `(<>)` definida para `b`. Esto es lo que garantiza que los resultados se combinan de acuerdo con las reglas de `Semigroup` para el tipo `b`.
   
   - **¿Por qué usamos `$`?**
     - El operador `$` se usa para evitar paréntesis adicionales y hacer que la expresión sea más legible. En este caso, se asegura de que todo lo que está a la derecha de `$` se ejecute y se pase correctamente como el argumento de la función `Combine`.

#### Ejemplo de uso

Ahora que hemos analizado la implementación, veamos cómo funciona con un ejemplo.

Supón que tenemos las siguientes instancias de `Combine`:

```haskell
import Data.Monoid (Sum(..))

f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)
```

Cada una de estas instancias de `Combine` envuelve una función de tipo `a -> b` (en este caso, `a` es de tipo `Int` y `b` es de tipo `Sum Int`).

- **`f`**: Es una función que toma un `n` y devuelve `Sum (n + 1)`.
- **`g`**: Es una función que toma un `n` y devuelve `Sum (n - 1)`.

Si ahora usamos `(<>)` para combinar `f` y `g`:

```haskell
h = f <> g
```

Entonces, la función interna de `h` será:

```haskell
unCombine h x = f x <> g x
```

Si aplicamos esto para `x = 1`:

```haskell
unCombine h 1 = f 1 <> g 1
              = Sum (1 + 1) <> Sum (1 - 1)
              = Sum 2 <> Sum 0
              = Sum 2
```

La combinación de `Sum 2` y `Sum 0` da como resultado `Sum 2` porque `Sum` es un `Semigroup` en el que `(<>)` suma los valores.

### Resumen

- **El tipo `Combine a b`** es un contenedor de funciones de tipo `a -> b`.
- **La operación `(<>)` para `Combine`** toma dos funciones `f` y `g`, y las combina creando una nueva función que, para un valor `x`, aplica `f` y `g` y luego combina sus resultados utilizando `(<>)` de `b`.
- **El operador `$`** en `Combine $ \x -> f x <> g x` simplemente se utiliza para hacer que la expresión sea más legible, evitando paréntesis adicionales.

Este patrón de combinar funciones en un contenedor como `Combine` es útil cuando quieres aplicar varias transformaciones a un valor y combinar sus resultados de manera estructurada y coherente.



## Sin Docker

### Comandos para ejecutar los archivos .hs, por ejemplo, cargar el archivo A.hs :

<code>
cd .\tarea-uno\
<br/> 
ghci .\a.hs
</code>

## Con Docker

<code>
docker build -t haskell-ghci .
<br/> 
docker run -it --rm haskell-ghci
</code>

## Ejecutar pruebas sobre los módulos:

Ver los comentarios en el código. __Estos deben borrarse al momento de la entrega__