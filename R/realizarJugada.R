# prompt: Crea una función quen dada una jugada seleccionada por la
# función mejor_jugada_minimax la realice en el tablero

realizar_jugada <- function(tablero, columna, jugador) {
  for (fila in 6:1) {
    if (tablero[fila, columna] == 0) {
      tablero[fila, columna] <- jugador
      return(tablero)
    }
  }
  # Si la columna está llena, no se realiza ninguna jugada
  return(tablero)
}

# Ejemplo de uso:
# Supongamos que la mejor jugada encontrada por mejor_jugada_minmax es la columna 3
# Y es el turno del jugador 2 (IA)

#mejor_jugada_IA <- mejor_jugada_minmax(tablero, 5, TRUE)
#if (!is.na(mejor_jugada_IA$jugada)) {
#  tablero <- realizar_jugada(tablero, mejor_jugada_IA$jugada, 2)
#}
