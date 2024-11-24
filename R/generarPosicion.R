#' Crear posición aleatoria
#'
#' @description crea un tablero aleatorio de cierta profundidad
#' @param tablero matriz 6 x 7 que representa la situación del tablero de juego.
#' @param turno
#' @return Una lista con los siguientes elementos
#' \itemize{
#'   \item tablero - matriz 6 x 7 que representa el tablero generado
#'   \item turnoUltimo - turno del jugador que ha realizado la última jugada en el tablero generado
#' }
#' @examples
#' tablero <- crear_posicion_aleatoria(20, 1)
#' visualizar_tablero(tablero$tablero)
#' tablero$turnoUltimo
#' sum(tablero == 1)
#' sum(tablero == 2)
crear_posicion_aleatoria <- function(profundidad = 10, turno = 1) {

  reiniciar <- TRUE

  while (reiniciar) {
    reiniciar <- FALSE

    turnoAux <- turno
    tablero <- reiniciar_tablero()


    for (i in 1:profundidad) {

      # se elige una jugada al azar
      jugadas_posibles <- jugadas_disponibles(tablero)

      jugada <- unlist(sample(as.list(jugadas_posibles), size = 1))
      tablero <- realizar_jugada(tablero, jugada, turnoAux)

      # validar que el juego ha terminado. Si es así, se reinicia el tablero
      if (juego_terminado(tablero) & i < 42) {
        reiniciar <- TRUE
        break

      }

      turnoAux <- (turnoAux %% 2) + 1

    }

  }
  print(sum(tablero == 1))
  print(sum(tablero == 2))
  print(turnoAux)
  return(list(tablero = tablero, turnoUltimo = turnoAux))

}
