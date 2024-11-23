#' Crear posición aleatoria
#'
#' @description dada una situacvión en el tablero de juego, devuelve las posibles
#'   jugadas existentes: columnas no completadas
#' @param tablero matriz 6 x 7 que representa la situación del tablero de juego.
#' @param turno
#' @examples
#' tablero <- .crear_posicion_aleatoria(42, 1)
#' visualizar_tablero(tablero)
.crear_posicion_aleatoria <- function(profundidad = 10, turno = 1) {


  repeat {

    turnoSiguiente <- turno
    tablero <- reiniciar_tablero()
    reiniciar <- FALSE

    for (i in 1:profundidad) {

      # validar que el juego ha terminado. Si es así, se reinicia el tablero
      if (juego_terminado(tablero)) {
        reiniciar <- TRUE
        evaluar_posicion(tablero, 1)
        break
      }

      # se elige una jugada al azar
      jugadas_posibles <- jugadas_disponibles(tablero)

      jugada <- sample(jugadas_posibles, 1)
      tablero <- realizar_jugada(tablero, jugada, turnoSiguiente)

      turnoSiguiente <- (turnoSiguiente %% 2) + 1

    }

    if (!reiniciar) {
      break
    }

  }

  print(i)
  return(tablero)

}
