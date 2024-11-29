# función que verifique condiciones de finalización del juego:
#  - o bien uno de los dos participantes ha hecho 4 casillas en linea
#  - o bien ya no quedan jugadas disponibles.
# Debe devolver False si el juego no ha alcanzado
# las condiciones de fin de juego o True en el caso de que sí

juego_terminado <- function(tablero) {
  # Verificar si hay 4 en línea para el jugador 1 o 2
  for (jugador in 1:2) {
    if (abs(evaluar_posicion(tablero, jugador)) >= 100000) {
      return(TRUE)
    }
  }

  # Verificar si hay jugadas disponibles
  if (length(jugadas_disponibles(tablero)) == 0) {
    return(TRUE)
  }

  # Si no se cumple ninguna de las condiciones anteriores, el juego no ha terminado
  return(FALSE)
}
