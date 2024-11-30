#' evaluua fin del juego y resultado
#'
#' @description evalua una posición mediante criterios estáticos basados en el
#' número de casillas conectadas del jugador en turno
#' @param tablero una matriz representado el estado del tablero de juego

#' @return devuelve una lista con el siguiente contenido
#' \item finalizado: booleano que representa el estado del juego: TRUE finalizado
#' \item resultado: resultado del juego
#' @details
#' si finalizado == TRUE, resultado contendrá "GANA HUMANO", "GANA IA" o "TABLAS"·
#' si finalizado = FALSE, resultado es NA
#' @examples
#' tablero <- crear_posicion_aleatoria(21)
#' juego_terminado(tablero$tablero)

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
