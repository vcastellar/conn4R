#' Crear posición aleatoria
#'
#' @description Crea un tablero aleatorio simulando \code{profundidad} jugadas
#'   legales al azar entre los dos jugadores (el humano abre). Si la secuencia
#'   produce una victoria antes de completar todas las jugadas, se descarta y se
#'   vuelve a generar, de modo que el tablero devuelto no es una posición
#'   terminal (salvo que el tablero se llene por completo).
#' @param profundidad número de jugadas que se simulan aleatoriamente
#' @return Matriz entera de 6 x 7 con la posición generada. Celdas: 0 vacío,
#'   1 humano, 2 IA.
#' @examples
#' tablero <- crear_posicion_aleatoria(21)
#' visualizar_tablero(tablero)
#' sum(tablero == 1)
#' sum(tablero == 2)
#' @export
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

  return(tablero)

}
