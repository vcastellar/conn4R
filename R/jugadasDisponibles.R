#' Jugadas disponibles y ordenación de candidatas
#'
#' @description
#' \code{jugadas_disponibles} devuelve las columnas no completadas del tablero.
#'
#' \code{ordenar_jugadas} ordena esas columnas de mejor a peor candidata para
#' maximizar la eficacia de la poda alpha-beta:
#' \enumerate{
#'   \item Victorias inmediatas del jugador en turno (prioridad máxima).
#'   \item Bloqueos de victoria inmediata del oponente.
#'   \item Resto de jugadas ordenadas por evaluación estática combinada.
#' }
#'
#' @param tablero Matriz 6x7 que representa el estado del tablero.
#' @param turno Jugador en turno: \code{1} (humano) o \code{2} (IA).
#'
#' @return
#' \code{jugadas_disponibles}: vector entero con los índices de columna (1-7)
#' que no están llenas.
#'
#' \code{ordenar_jugadas}: data.frame con columnas \code{jugadas} (columna 1-7)
#' y \code{puntuacion} (heurística de ordenación), ordenado de mayor a menor
#' puntuación.
#'
#' @examples
#' tablero <- crear_posicion_aleatoria(11)
#' visualizar_tablero(tablero)
#' jugadas_disponibles(tablero)
#' ordenar_jugadas(tablero, turno = 2)



jugadas_disponibles <- function(tablero) {
  # Encuentra las columnas que no están llenas
  jugadas_candidatas <- which(tablero[1, ] == 0)
  return(jugadas_candidatas)
}


ordenar_jugadas <- function(tablero, turno) {
  jugadas_candidatas <- jugadas_disponibles(tablero)

  n <- length(jugadas_candidatas)
  puntuaciones <- numeric(n)

  oponente <- ifelse(turno == 1L, 2L, 1L)

  # Prioridad máxima: victoria inmediata o bloqueo de victoria rival
  WIN_SCORE  <- 1e9
  BLOCK_SCORE <- 1e8

  for (i in seq_len(n)) {
    col <- jugadas_candidatas[i]

    tablero_j <- realizar_jugada(tablero, col, turno)
    if (juego_terminado(tablero_j)$finalizado && !is.na(juego_terminado(tablero_j)$resultado) && juego_terminado(tablero_j)$resultado == turno) {
      puntuaciones[i] <- WIN_SCORE
      next
    }

    tablero_o <- realizar_jugada(tablero, col, oponente)
    if (juego_terminado(tablero_o)$finalizado && !is.na(juego_terminado(tablero_o)$resultado) && juego_terminado(tablero_o)$resultado == oponente) {
      puntuaciones[i] <- BLOCK_SCORE
      next
    }

    eval_j <- evaluar_posicion(tablero_j)
    eval_o <- evaluar_posicion(tablero_o)
    puntuaciones[i] <- abs(eval_j) + abs(eval_o)
  }

  df <- data.frame(jugadas = jugadas_candidatas, puntuacion = puntuaciones)
  df <- df[order(df$puntuacion, decreasing = TRUE), ]

  return(df)
}
