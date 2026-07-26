#' Iniciar una partida de Conecta 4
#'
#' @description Punto de entrada principal del paquete. Inicia una partida de
#'   Conecta 4 usando el motor C++ compilado via Rcpp para el cálculo minimax,
#'   la evaluación estática y la detección de fin de partida. La interfaz
#'   (visualización, entrada del usuario, profundidad adaptativa) se gestiona
#'   desde R.
#'
#' @param profundidad Profundidad de búsqueda del algoritmo minimax. Por defecto 5.
#' @param turno Jugador que comienza: 1 para humano, 2 para IA. Por defecto 1.
#' @param profAdaptative Lógico. Si \code{TRUE} ajusta la profundidad
#'   dinámicamente según las columnas disponibles. Por defecto \code{TRUE}.
#' @param auto Lógico. Si \code{TRUE} la IA juega contra sí misma sin
#'   intervención humana. Por defecto \code{FALSE}.
#'
#' @return La matriz 6×7 con el estado final del tablero (invisible).
#'
#' @examples
#' \dontrun{
#' # Partida humano vs IA (humano primero, profundidad 6)
#' iniciar_partida(profundidad = 6)
#'
#' # La IA abre la partida
#' iniciar_partida(turno = 2)
#'
#' # IA contra sí misma
#' iniciar_partida(auto = TRUE, profundidad = 5)
#' }
#'
#' @seealso \code{\link{minimax}}, \code{\link{evaluar_posicion}}
#'
#' @export
iniciar_partida <- function(profundidad = 5, turno = 1,
                            profAdaptative = TRUE, auto = FALSE) {

  tablero  <- reiniciar_tablero()
  p        <- visualizar_tablero(tablero)
  print(p)
  i        <- 1
  j        <- 0
  resultado <- "DRAW"

  # Helpers internos que llaman al motor C++
  ia_mueve <- function(tab, prof, maximizandoIA) {
    tik <- system.time({
      res <- minimax(tab, prof, maximizandoIA)
    })
    list(jugada     = res$jugada,
         puntuacion = res$puntuacion,
         nodos      = res$nodos,
         variante   = res$variante,
         tiempo     = tik[[3]])
  }

  fin_turno <- function(tab, turno_actual, prof, max_ia) {
    res <- ia_mueve(tab, prof, max_ia)
    tab <- realizar_jugada(tab, res$jugada, turno_actual)
    p   <- visualizar_tablero(tab)
    print(p)
    cat("\n")
    nods_s <- if (res$tiempo > 0) round(res$nodos / res$tiempo) else NA
    cat("-----------------------------------------------------------------\n")
    cat(sprintf("valoracion:       %d\n",     res$puntuacion))
    cat(sprintf("jugada realizada: %d\n",     res$jugada))
    cat(sprintf("profundidad:      %d\n",     prof))
    cat(sprintf("nodos evaluados:  %.0f\n",   res$nodos))
    cat(sprintf("nod/s:            %.0f\n",   nods_s))
    cat(sprintf("tiempo:           %.2f s\n", res$tiempo))
    cat(sprintf("variante:         %s\n",     paste(res$variante, collapse = " -> ")))
    cat("-----------------------------------------------------------------\n\n")
    tab
  }

  terminado <- function(tab) {
    juego_terminado(tab)
  }

  # Modo humano vs IA
  if (!auto) {
    if (turno == 2) {
      prof    <- if (profAdaptative) .adaptativa(tablero, profundidad) else profundidad
      tablero <- fin_turno(tablero, 2L, prof, TRUE)
      fin     <- terminado(tablero)
      if (fin$finalizado) { cat("gana IA\n"); return(invisible(tablero)) }
      i <- i + 1; j <- 1
    }

    while (i <= (42 - j)) {
      tablero <- turno_humano(tablero)
      p       <- visualizar_tablero(tablero)
      print(p)
      cat(sprintf("valoracion HU: %d\n", evaluar_posicion(tablero)))

      fin <- terminado(tablero)
      if (fin$finalizado) { resultado <- fin$resultado; break }
      i <- i + 1

      prof    <- if (profAdaptative) .adaptativa(tablero, profundidad) else profundidad
      tablero <- fin_turno(tablero, 2L, prof, TRUE)

      fin <- terminado(tablero)
      if (fin$finalizado) { resultado <- fin$resultado; break }
      i <- i + 1
    }

    cat(sprintf("Resultado: %s\n", resultado))
  }

  # Modo automático (IA vs IA)
  if (auto) {
    while (i <= 42) {
      turno_actual <- ((i - 1) %% 2) + 1
      max_ia       <- (turno_actual == 2)
      prof         <- if (profAdaptative) .adaptativa(tablero, profundidad) else profundidad

      tablero <- fin_turno(tablero, turno_actual, prof, max_ia)

      fin <- terminado(tablero)
      if (fin$finalizado) {
        resultado <- switch(as.character(fin$resultado),
                            "1" = "GANA HUMANO (jugador 1)",
                            "2" = "GANA IA (jugador 2)",
                            "0" = "EMPATE")
        cat(sprintf("Resultado: %s\n", resultado))
        break
      }
      i <- i + 1
    }
  }

  invisible(tablero)
}
