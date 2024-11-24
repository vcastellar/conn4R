# función que mediante un algoritmo mini-max la IA decide cuál es su mejor
# jugada dada una cierta posición del tablero para conseguir la victoria en el
#  desarrollo de la partida hasta su finalización.
# La evaluacion de la posición la debe hacer la función evaluate_position anterior
# prueba

mejor_jugada_minmax <- function(tablero, profundidad, maximizando_jugador,
                                alpha = -Inf, beta = Inf) {

  if (profundidad == 0 || juego_terminado(tablero)) {
    return(list(puntuacion = evaluar_posicion(tablero, 2) + evaluar_posicion(tablero, 1),
                jugada = NA))
  }

  if (maximizando_jugador) {
    mejor_puntuacion <- -Inf
    mejor_jugada <- NA

    for (columna in jugadas_disponibles(tablero)) {
      nuevo_tablero <- realizar_jugada(tablero, columna, 2)


      puntuacion <- mejor_jugada_minmax(nuevo_tablero, profundidad - 1, FALSE,
                                        alpha, beta)$puntuacion

      if (puntuacion > mejor_puntuacion) {
        mejor_puntuacion <- puntuacion
        mejor_jugada <- columna
      }

      alpha <- max(alpha, mejor_puntuacion)
      if (beta <= alpha) {
        break  # Poda alpha-beta
      }


    }


    return(list(puntuacion = mejor_puntuacion, jugada = mejor_jugada))

  } else {

    mejor_puntuacion <- Inf
    mejor_jugada <- NA


    for (columna in jugadas_disponibles(tablero)) {
      nuevo_tablero <- realizar_jugada(tablero, columna, 1)

      puntuacion <- mejor_jugada_minmax(nuevo_tablero, profundidad - 1, TRUE,
                                        alpha, beta)$puntuacion

      if (puntuacion < mejor_puntuacion) {
        mejor_puntuacion <- puntuacion
        mejor_jugada <- columna

      }
      beta <- min(beta, mejor_puntuacion)
      if (beta <= alpha) {
        break  # Poda alpha-beta
      }

    }

  }

  return(list(puntuacion = mejor_puntuacion, jugada = mejor_jugada))
}
