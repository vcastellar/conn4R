#' Iniciar una partida usando el motor C++
#'
#' @description Versión de \code{iniciar_partida} que delega el cálculo minimax,
#'   la evaluación estática y la detección de fin de partida al motor C++ compilado.
#'   El resto de la interfaz (visualización, entrada del usuario, profundidad adaptativa)
#'   es idéntica a la versión R.
#'
#' @param profundidad Profundidad de búsqueda del algoritmo minimax. Por defecto 5.
#' @param turno Jugador que comienza: 1 para humano, 2 para IA. Por defecto 1.
#' @param profAdaptative Lógico. Si TRUE ajusta la profundidad dinámicamente según
#'   las columnas disponibles. Por defecto TRUE.
#' @param auto Lógico. Si TRUE la IA juega contra sí misma sin intervención humana.
#'   Por defecto FALSE.
#'
#' @return La matriz 6×7 con el estado final del tablero (invisible).
#'
#' @examples
#' # Partida humano vs IA (humano primero)
#' iniciar_partida_cpp(profundidad = 6)
#'
#' # IA contra sí misma
#' iniciar_partida_cpp(auto = TRUE, profundidad = 5)
#'
#' @export
iniciar_partida_cpp <- function(profundidad = 5, turno = 1,
                                profAdaptative = TRUE, auto = FALSE) {

  tablero  <- reiniciar_tablero()
  p        <- visualizar_tablero(tablero)
  print(p)
  i        <- 1
  j        <- 0
  resultado <- "DRAW"

  # --------------------------------------------------------------------------
  # Helpers internos que llaman al motor C++
  # --------------------------------------------------------------------------

  ia_mueve <- function(tab, prof, maximizandoIA) {
    tik <- system.time({
      res <- minimax_r(tab, prof, maximizandoIA)
    })
    list(jugada = res$jugada, puntuacion = res$puntuacion, tiempo = tik[[3]])
  }

  fin_turno <- function(tab, turno_actual, prof, max_ia) {
    res <- ia_mueve(tab, prof, max_ia)
    tab <- realizar_jugada(tab, res$jugada, turno_actual)
    p   <- visualizar_tablero(tab)
    print(p)
    cat("\n")
    cat("-----------------------------------------------------------------\n")
    cat(sprintf("valoracion:       %d\n",   res$puntuacion))
    cat(sprintf("jugada realizada: %d\n",   res$jugada))
    cat(sprintf("profundidad:      %d\n",   prof))
    cat(sprintf("tiempo:           %.2f s\n", res$tiempo))
    cat("-----------------------------------------------------------------\n\n")
    tab
  }

  terminado <- function(tab) {
    juego_terminado_cpp(tab)
  }

  # --------------------------------------------------------------------------
  # Modo humano vs IA
  # --------------------------------------------------------------------------
  if (!auto) {

    # Si la IA abre la partida
    if (turno == 2) {
      prof    <- if (profAdaptative) .adaptativa(tablero, profundidad) else profundidad
      tablero <- fin_turno(tablero, 2L, prof, TRUE)
      fin     <- terminado(tablero)
      if (fin$finalizado) { cat("gana IA\n"); return(invisible(tablero)) }
      i <- i + 1; j <- 1
    }

    while (i <= (42 - j)) {

      # Turno humano
      tablero <- turno_humano(tablero)
      p       <- visualizar_tablero(tablero)
      print(p)
      cat(sprintf("valoracion HU: %d\n", evaluar_posicion_cpp(tablero)))

      fin <- terminado(tablero)
      if (fin$finalizado) { resultado <- fin$resultado; break }
      i <- i + 1

      # Turno IA
      prof    <- if (profAdaptative) .adaptativa(tablero, profundidad) else profundidad
      tablero <- fin_turno(tablero, 2L, prof, TRUE)

      fin <- terminado(tablero)
      if (fin$finalizado) { resultado <- fin$resultado; break }
      i <- i + 1
    }

    cat(sprintf("Resultado: %s\n", resultado))
  }

  # --------------------------------------------------------------------------
  # Modo automático (IA vs IA)
  # --------------------------------------------------------------------------
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
