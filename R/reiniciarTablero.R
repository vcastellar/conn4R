# crea una estructura de datos que permita representar un tablero de conecta 4 de 7 columnas y 5 filas

# Creamos una matriz de 7 columnas y 6 filas llena de 0
reiniciar_tablero <- function() {
  tablero <- matrix(0, nrow = 6, ncol = 7)
}


# 0 representa casilla vacía
# 1 representa ficha del jugador 1
# 2 representa ficha del jugador 2
