# función que dada una jugada seleccionada por la modificvavcion
# función mejor_jugada_minimax la realice en el tablero
# prueba

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
