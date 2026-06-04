## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width  = 6,
  fig.height = 5
)
set.seed(42)

## ----library------------------------------------------------------------------
library(conn4R)

## ----tablero-vacio------------------------------------------------------------
tablero <- reiniciar_tablero()
dim(tablero)   # 6 filas x 7 columnas
visualizar_tablero(tablero)

## ----posicion-aleatoria-------------------------------------------------------
tablero <- crear_posicion_aleatoria(14)
visualizar_tablero(tablero)

# Recuento de piezas
cat("Piezas humano (1):", sum(tablero == 1), "\n")
cat("Piezas IA    (2):", sum(tablero == 2), "\n")

## ----posicion-manual----------------------------------------------------------
t <- reiniciar_tablero()
# Jugadas alternadas: humano en columnas 4, 3, 5 y IA en 4, 4, 4
t <- realizar_jugada(t, 4, 1)
t <- realizar_jugada(t, 4, 2)
t <- realizar_jugada(t, 3, 1)
t <- realizar_jugada(t, 4, 2)
t <- realizar_jugada(t, 5, 1)
t <- realizar_jugada(t, 4, 2)
visualizar_tablero(t)

