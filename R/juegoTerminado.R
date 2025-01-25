#' end-of-game evaluation and result
#'
#' @description evaluates whether the game has ended and with what result 
#' ("HUMAN WINS", "AI WINS", "DRAW").
#' @param tablero a matrix representing the state of the game board

#' @return returns a list with the following contents
#' \itemize{
  #' \item{finalizado}: boolean representing the state of the game: TRUE game finished
  #' \item{resultado}: outcome of the game. If the game is over, there are three options
#' }
#' @details
#' \itemize{
  #' \item{If the game is over is TRUE}: result can be "WIN HUMAN", "WIN IA" or "DRAW’.
  #' \item{If the game is over is TRUE}: result is NA
#' }
#' @examples
#' tablero <- crear_posicion_aleatoria(21)
#' p <- visualizar_tablero(tablero)
#' print(p)
#' juego_terminado(tablero)

juego_terminado <- function(tablero) {
  # Verificar si hay 4 en línea para el jugador 1 o 2
  for (jugador in 1:2) {
    if (abs(.evaluar_turno(tablero, jugador)) >= 50000) {
      if (jugador == 1) {
        return(list(finalizado = TRUE, resultado = "WIN HUMAN"))
      } else {
        return(list(finalizado = TRUE, resultado = "WIN IA"))
      }
    }
  }

  # Verificar si hay jugadas disponibles
  if (length(jugadas_disponibles(tablero)) == 0) {
    return(list(finalizado = TRUE, resultado = "DRAW"))
  }

  # Si no se cumple ninguna de las condiciones anteriores, el juego no ha terminado
  return(list(finalizado = FALSE, resultado = NA))
}
