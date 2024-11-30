# función que verifique condiciones de finalización del juego:
#  - o bien uno de los dos participantes ha hecho 4 casillas en linea
#  - o bien ya no quedan jugadas disponibles.
# Debe devolver False si el juego no ha alcanzado
# las condiciones de fin de juego o True en el caso de que sí

juego_terminado <- function(tablero) {
  # Verificar si hay 4 en línea para el jugador 1 o 2
  for (jugador in 1:2) {
    if (abs(evaluar_posicion(tablero, jugador)) >= 100000) {
      if (jugador == 1) {
        return(list(finalizado = TRUE, resultado = "GANA HUMANO"))
      } else {
        return(list(finalizado = TRUE, resultado = "GANA IA"))
      }
    }
  }

  # Verificar si hay jugadas disponibles
  if (length(jugadas_disponibles(tablero)) == 0) {
    return(list(finalizado = TRUE, resultado = "TABLAS"))
  }

  # Si no se cumple ninguna de las condiciones anteriores, el juego no ha terminado
  return(list(finalizado = FALSE, resultado = NA))
}
