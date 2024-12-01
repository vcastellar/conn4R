#' Crear posición aleatoria
#'
#' @description crea un tablero aleatorio de cierta profundidad
#' @param profundidad número de jugadas que se simulan aleatoriamente
#' @return Una lista con los siguientes elementos
#' \itemize{
#'   \item tablero - matriz 6 x 7 que representa el tablero generado
#'   \item turnoUltimo - turno del jugador que ha realizado la última jugada en el tablero generado
#' }
#' @examples
#' tablero <- crear_posicion_aleatoria(21)
#' visualizar_tablero(tablero$tablero)
#' tablero$turnoUltimo
#' sum(tablero$tablero == 1)
#' sum(tablero$tablero == 2)
crear_posicion_aleatoria <- function(profundidad = 10) {

  reiniciar <- TRUE

  while (reiniciar) {
    reiniciar <- FALSE

    turno <- 1
    tablero <- reiniciar_tablero()


    for (i in 1:profundidad) {

      # se elige una jugada al azar
      jugadas_posibles <- jugadas_disponibles(tablero)

      jugada <- unlist(sample(as.list(jugadas_posibles), size = 1))
      tablero <- realizar_jugada(tablero, jugada, turno)

      # validar que el juego ha terminado. Si es así, se reinicia el tablero
      if (juego_terminado(tablero)$finalizado & i < 42) {
        reiniciar <- TRUE
        break

      }

      turno <- (turno %% 2) + 1

    }

  }

  return(list(tablero = tablero, turnoUltimo = turno))

}
