# Infiere qué jugador tiene el turno a partir del número de fichas del tablero,
# asumiendo que el humano (jugador 1) abre la partida. Devuelve NA si el
# recuento de fichas no corresponde a una partida legal humano-primero.
.turno_a_jugar <- function(tablero) {
  n1 <- sum(tablero == 1L)
  n2 <- sum(tablero == 2L)
  if (n1 == n2)      return(1L)
  if (n1 == n2 + 1L) return(2L)
  NA_integer_
}

.veredicto_analisis <- function(puntuacion, fin) {
  if (isTRUE(fin$finalizado)) {
    return(switch(as.character(fin$resultado),
      "1" = "Partida terminada: gana el humano (jugador 1).",
      "2" = "Partida terminada: gana la IA (jugador 2).",
      "0" = "Partida terminada: empate.",
      "Partida terminada."))
  }
  if (puntuacion >=  1e9) return("Mate forzado a favor de la IA (jugador 2).")
  if (puntuacion <= -1e9) return("Mate forzado a favor del humano (jugador 1).")
  if (puntuacion > 0)     return("Ventaja para la IA (jugador 2).")
  if (puntuacion < 0)     return("Ventaja para el humano (jugador 1).")
  "Posición equilibrada."
}

#' Analizar una posición con el motor minimax
#'
#' @description Interfaz de alto nivel sobre \code{\link{minimax}} pensada para
#'   estudiar una posición: calcula la jugada recomendada para el jugador en
#'   turno, la puntuación minimax, la evaluación estática y la variante
#'   principal, y las envuelve en un objeto imprimible con un veredicto legible.
#'   A diferencia de \code{minimax}, no hay que traducir manualmente el turno a
#'   \code{maximizandoIA}: basta indicar quién mueve (o dejar que se infiera).
#'
#' @param tablero Matriz de 6 x 7 con la posición a analizar. Celdas: 0 vacío,
#'   1 humano, 2 IA. Puede construirse con \code{\link{crear_posicion}}.
#' @param turno Jugador que tiene el turno: \code{1} (humano) o \code{2} (IA).
#'   Si es \code{NULL} (valor por defecto) se infiere del número de fichas del
#'   tablero, asumiendo que el humano abre la partida.
#' @param profundidad Profundidad máxima de búsqueda del minimax. Por defecto
#'   \code{7}.
#' @param mostrar Lógico. Si \code{TRUE}, reproduce la variante principal sobre
#'   el tablero mediante \code{\link{visualizar_variante}}. Por defecto
#'   \code{FALSE}.
#' @param lapso Segundos de espera entre posiciones cuando \code{mostrar = TRUE}.
#'   Por defecto \code{1}.
#'
#' @return Un objeto de clase \code{"analisis_conn4R"} (una lista) con, entre
#'   otros, los elementos:
#' \describe{
#'   \item{turno}{Jugador analizado (1 o 2).}
#'   \item{profundidad}{Profundidad de búsqueda empleada.}
#'   \item{jugada}{Columna recomendada (1-7), o \code{NA} si no hay jugada.}
#'   \item{puntuacion}{Puntuación minimax (perspectiva de la IA: positivo =
#'     ventaja IA).}
#'   \item{evaluacion}{Evaluación estática de la posición.}
#'   \item{nodos}{Nodos evaluados durante la búsqueda.}
#'   \item{variante}{Variante principal (secuencia de columnas).}
#'   \item{veredicto}{Interpretación textual de la puntuación.}
#' }
#'   El objeto dispone de un método \code{print} que muestra un resumen legible.
#'
#' @examples
#' # Introducir una posición y analizarla (turno inferido automáticamente)
#' tablero <- crear_posicion(c(4, 4, 3, 5, 3))
#' analisis <- analizar_posicion(tablero, profundidad = 5)
#' analisis
#' analisis$jugada
#'
#' # Indicar el turno explícitamente
#' analizar_posicion(tablero, turno = 1, profundidad = 4)
#'
#' @seealso \code{\link{minimax}}, \code{\link{crear_posicion}},
#'   \code{\link{visualizar_variante}}
#'
#' @export
analizar_posicion <- function(tablero, turno = NULL, profundidad = 7,
                              mostrar = FALSE, lapso = 1) {
  if (!is.matrix(tablero) || !identical(dim(tablero), c(6L, 7L)) ||
      anyNA(tablero) || any(!tablero %in% 0:2)) {
    stop("`tablero` debe ser una matriz 6x7 con valores 0, 1 o 2.",
         call. = FALSE)
  }
  if (length(profundidad) != 1L || is.na(profundidad) ||
      !is.numeric(profundidad) || !is.finite(profundidad) ||
      profundidad != as.integer(profundidad) || profundidad < 0) {
    stop("`profundidad` debe ser un entero mayor o igual que 0.", call. = FALSE)
  }
  if (length(mostrar) != 1L || is.na(mostrar) || !is.logical(mostrar)) {
    stop("`mostrar` debe ser TRUE o FALSE.", call. = FALSE)
  }

  tablero <- matrix(as.integer(tablero), nrow = 6L, ncol = 7L)

  if (is.null(turno)) {
    turno <- .turno_a_jugar(tablero)
    if (is.na(turno)) {
      stop("No se puede inferir el turno de esta posición; ",
           "indica `turno` (1 = humano, 2 = IA).", call. = FALSE)
    }
  }
  if (length(turno) != 1L || is.na(turno) || !turno %in% c(1, 2)) {
    stop("`turno` debe ser 1 (humano) o 2 (IA).", call. = FALSE)
  }

  turno <- as.integer(turno)
  prof  <- as.integer(profundidad)
  maximizandoIA <- (turno == 2L)

  fin <- juego_terminado(tablero)
  res <- minimax(tablero, prof, maximizandoIA)

  analisis <- list(
    turno          = turno,
    profundidad    = prof,
    jugada         = res$jugada,
    puntuacion     = res$puntuacion,
    evaluacion     = evaluar_posicion(tablero),
    nodos          = res$nodos,
    nodos_normales = res$nodos_normales,
    nodos_tacticos = res$nodos_tacticos,
    variante       = res$variante,
    terminado      = isTRUE(fin$finalizado),
    resultado      = fin$resultado,
    veredicto      = .veredicto_analisis(res$puntuacion, fin)
  )
  class(analisis) <- "analisis_conn4R"

  if (isTRUE(mostrar) && length(res$variante) > 0L) {
    visualizar_variante(tablero, turno, res$variante, lapso = lapso)
  }

  analisis
}

#' @description \code{print.analisis_conn4R} muestra un resumen legible del
#'   análisis de una posición.
#' @param x Objeto de clase \code{"analisis_conn4R"} devuelto por
#'   \code{analizar_posicion}.
#' @param ... Argumentos adicionales (ignorados).
#' @rdname analizar_posicion
#' @export
print.analisis_conn4R <- function(x, ...) {
  quien <- if (x$turno == 1L) "humano (jugador 1)" else "IA (jugador 2)"
  regla <- strrep("-", 57L)

  cat("Análisis de la posición (motor minimax C++)\n")
  cat(regla, "\n", sep = "")
  cat(sprintf("Turno de:            %s\n", quien))
  cat(sprintf("Profundidad:         %d\n", x$profundidad))
  if (isTRUE(x$terminado)) {
    cat("Estado:              la partida ya ha terminado\n")
  }
  cat(sprintf("Jugada recomendada:  %s\n",
              if (is.na(x$jugada)) "ninguna" else sprintf("columna %d", x$jugada)))
  cat(sprintf("Puntuación minimax:  %.0f\n", as.double(x$puntuacion)))
  cat(sprintf("Evaluación estática: %d\n", as.integer(x$evaluacion)))
  cat(sprintf("Veredicto:           %s\n", x$veredicto))
  cat(sprintf("Nodos evaluados:     %.0f\n", as.double(x$nodos)))
  cat(sprintf("Variante principal:  %s\n",
              if (length(x$variante) == 0L) "(sin variante)"
              else paste(x$variante, collapse = " -> ")))
  cat(regla, "\n", sep = "")

  invisible(x)
}
