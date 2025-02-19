# Práctica 1

## Lógica computacional

### Integrantes

- Carrillo Benítez Valentina

- Gómez Calva Carlos Manuel

### Explicación de las funciones

1. **firstVowels**

    En el caso base (cadena vacía) se regresa una cadena vacía, en el caso recursivo se separan las vocales y consonantes, y se concatenan las vocales al inicio seguido de las consonantes.

2. **isAnagram**

    En el caso base (ambas cadenas vacías) se regresa true, en el caso recursivo se convierten las cadenas a minúsculas, se eliminan los espacios y se comparan las cadenas ordenadas alfabéticamente.

3. **commonSuffix**

    En el caso base (lista vacía) se regresa una cadena vacía, en el caso recursivo se invierten las cadenas, se encuentra el prefijo común usando foldl1, y luego se revierte el resultado para obtener el sufijo común.

4. **interseccion**

    En el caso base (una lista vacía) se regresa una lista vacía, en el caso recursivo se recorren los elementos de la segunda lista y se filtran los que también están en la primera lista.

5. **ackerman**

    En el caso base (m = 0) se regresa n + 1, en el caso recursivo:

    Si m > 0 y n = 0, se llama recursivamente con (m-1, 1).
    Si m > 0 y n > 0, se llama primero con (m, n-1) y luego con (m-1, resultado).

6. **quicksort**

    En el caso base se retorna la lista vacía, en el caso recursivo se concatena el pivote (variable piv) a las listas min y max, sobre las que se hace recursión.

7. **bTreeInsert**

    En el caso base (árbol vacío) se construye un árbol cuya raíz será el elemento del parámetro __a__, en el caso recursivo se añade __a__ al sub-árbol derecho o izquierdo haciendo recursión sobre este.

8. **bTreeSearch**

    En el caso base (árbol vacío) se regresa falso, en el caso recursivo, si el valor en la raíz es el buscado, regresa true, caso contrario hace recursión sobre los sub-árboles derecho e izquierdo.

9. **bTreeMap**

    En el caso base (árbol vacío) se regresa el árbol vacío, en el caso recursivo se aplica la función a la raíz y hace recursión sobre los sub-árboles derecho e izquierdo.

10. **bTreeHeight**

    En el caso base (árbol vacío) se regresa 0, en el caso recursivo se suma 1 a la altura del sub-árbol más "alto".