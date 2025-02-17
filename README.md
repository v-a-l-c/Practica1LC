# Práctica 1

## Lógica computacional

### Integrantes

- Carrillo Benítez Valentina

- Gómez Calva Carlos Manuel

### Explicación de las funciones

1. **firstVowels**
2. **isAnagram**
3. **commonSuffix**
4. **interseccion**
5. **ackerman**
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