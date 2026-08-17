# conn4R 0.2.0

## Nuevas funciones

* `crear_posicion()` construye un tablero a partir de una secuencia de jugadas
  (columnas), validando su legalidad. Es la vía recomendada para *introducir una
  posición* que después se quiera analizar o continuar.
* `analizar_posicion()` es una interfaz de alto nivel sobre `minimax()`:
  devuelve la jugada recomendada, la puntuación, la variante principal y un
  veredicto legible, infiere el turno cuando no se indica y, opcionalmente,
  reproduce la variante principal. Se acompaña de un método `print()`.

## Preparación para CRAN

* Se sustituye `library(ggplot2)` dentro de `visualizar_tablero()` por
  importaciones explícitas del espacio de nombres (`@importFrom`).
* Se añade documentación a nivel de paquete (`?conn4R`).
* La metainformación de `DESCRIPTION` pasa a usar `Authors@R` e incorpora los
  campos `URL` y `BugReports`.
* Se elimina la página de ayuda obsoleta `hello`.
* Se corrige la sección `\value` de `crear_posicion_aleatoria()`, que ahora
  describe correctamente que devuelve una matriz.

# conn4R 0.1.3

* Motor de Conecta 4 con minimax, poda alfa-beta, tabla de transposición y
  búsqueda de quiescencia implementado en C++ (Rcpp).
