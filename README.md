# conn4R

<!-- badges: start -->
<!-- badges: end -->

**conn4R** es un paquete de R para jugar y analizar partidas de **Conecta 4**
contra una inteligencia artificial. El motor de búsqueda está escrito en C++
(via [Rcpp](https://www.rcpp.org/)) e implementa **minimax con poda alfa-beta**,
tabla de transposición, búsqueda de quiescencia y una evaluación estática de
posición. La capa R aporta la interfaz de juego, la introducción y el análisis
de posiciones y la visualización con `ggplot2`.

## Instalación

```r
# install.packages("remotes")
remotes::install_github("vcastellar/conn4r")
```

El paquete necesita un compilador de C++ (incluido en Rtools en Windows o en
las herramientas de desarrollo del sistema en macOS/Linux).

## Convenio del tablero

El tablero es una matriz entera de 6 filas por 7 columnas. Cada celda vale
`0` (vacía), `1` (ficha del humano) o `2` (ficha de la IA). Las fichas caen por
gravedad hasta la fila libre más baja de la columna.

## Uso

Las tres tareas principales tienen una función de acceso directo.

### 1. Jugar una partida

```r
library(conn4R)

# Humano contra IA (el humano empieza, profundidad 6)
iniciar_partida(profundidad = 6)

# La IA abre la partida
iniciar_partida(turno = 2)

# La IA juega contra sí misma
iniciar_partida(auto = TRUE)
```

### 2. Introducir una posición

`crear_posicion()` construye un tablero reproduciendo una secuencia de jugadas
(columnas) que alternan entre los jugadores, validando que todas sean legales.

```r
# Apertura central y respuestas alternas (el humano abre)
tablero <- crear_posicion(c(4, 4, 3, 5, 3))
visualizar_tablero(tablero)
```

### 3. Analizar la posición con el minimax

`analizar_posicion()` calcula la jugada recomendada, la puntuación, la variante
principal y un veredicto legible. El turno puede indicarse o inferirse.

```r
analisis <- analizar_posicion(tablero, profundidad = 8)
analisis
#> Análisis de la posición (motor minimax C++)
#> ---------------------------------------------------------
#> Turno de:            IA (jugador 2)
#> Profundidad:         8
#> Jugada recomendada:  columna 4
#> ...

analisis$jugada     # columna recomendada
analisis$variante   # variante principal

# Reproducir la variante principal sobre el tablero
analizar_posicion(tablero, profundidad = 8, mostrar = TRUE)
```

## Funciones principales

| Función | Descripción |
|---|---|
| `iniciar_partida()` | Jugar una partida completa (humano vs IA o IA vs IA). |
| `crear_posicion()` | Introducir una posición a partir de una secuencia de jugadas. |
| `analizar_posicion()` | Analizar una posición con el motor minimax. |
| `minimax()` | Motor de búsqueda de bajo nivel (C++). |
| `evaluar_posicion()` | Evaluación estática de una posición (C++). |
| `visualizar_tablero()` / `visualizar_variante()` | Dibujar posiciones y variantes. |
| `reiniciar_tablero()` | Crear un tablero vacío. |
| `crear_posicion_aleatoria()` | Generar una posición aleatoria legal. |

## Licencia

GPL-3.
