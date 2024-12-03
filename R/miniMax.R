#' algoritmo minimax
#'
#' @description función que mediante un algoritmo mini-max la IA decide cuál es su mejor
#' jugada dada una cierta posición del tablero 

#' @param tablero a matrix representing the state of the game board
#' @param profundidad un entero que fija la profundidad del árbol de jugadas a analizar
#' @param maximizandoIA Booleano. TRUE significa que se maximiza la puntuación de la IA
#'   FALSE se minimiza la puntuación del jugador humano
#' @param alpha parámetro de la poda alpha-beta. Por defecto -Inf
#' @param beta parámetro de la poda alpha-beta. Por defecto +Inf

#' @return returns a list with the following contents
#' \itemize{
#' \item{puntuacion}: puntuación optenida al evaluar la posición al realizar la 'jugada'
#' \item{jugada}: jugada elegida por el algoritmo
#' }
#' @details
#' \itemize{
#' \item{If the game is over is TRUE}: result can be "WIN HUMAN", "WIN IA" or "DRAW’.
#' \item{If the game is over is TRUE}: result is NA
#' }
#' @examples
#' tablero <- crear_posicion_aleatoria(3)
#' numNodos <- 0
#' ts <- system.time(
#'   minimax(tablero = tablero, profundidad = 7, maximizandoIA = TRUE)
#' )
#' numNodos
#' numNodos /ts[3]

minimax <- function(tablero, profundidad, maximizandoIA,
                                alpha = -Inf, beta = Inf) {

  if (profundidad == 0 || juego_terminado(tablero)$finalizado) {
    return(list(puntuacion = evaluar_posicion(tablero), jugada = NA))
  }

  if (maximizandoIA) {
    mejor_puntuacion <- -Inf
    mejor_jugada <- NA

    for (columna in jugadas_disponibles(tablero)) {
      nuevo_tablero <- realizar_jugada(tablero, columna, 2)

      puntuacion <- minimax(nuevo_tablero, profundidad - 1, FALSE, alpha, beta)$puntuacion

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

      puntuacion <- minimax(nuevo_tablero, profundidad - 1, TRUE, alpha, beta)$puntuacion

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
