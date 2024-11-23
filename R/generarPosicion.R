#' Crear posición aleatoria
#'
#' @description dada una situacvión en el tablero de juego, devuelve las posibles
#'   jugadas existentes: columnas no completadas
#' @param tablero matriz 6 x 7 que representa la situación del tablero de juego.
#' @param turno
#' @examples
#' tablero <- crear_posicion_aleatoria()
#' visualizar_tablero(tablero)
#'
crear_posicion_aleatoria <- function(profundidad = 10, turno = 1) {
  i <- 0
  turnoSiguiente <- turno
  tablero <- reiniciar_tablero()
  for (i in 1:profundidad) {

    # validar que el juego ha terminado. Si es así, se reinicia el tablero
    if (juego_terminado(tablero) & i < profundidad) {
      tablero <- reiniciar_tablero()
      i <- 0
      turnoSiguiente <- turno
      break
    }

    # se elige una jugada al azar
    jugadas_posibles <- jugadas_disponibles(tablero)

    jugada <- sample(jugadas_posibles, 1)
    tablero <- realizar_jugada(tablero, jugada, turnoSiguiente)

    turnoSiguiente <- (turnoSiguiente %% 2) + 1

  }

  return(tablero)

}
