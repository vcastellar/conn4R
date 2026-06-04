#' Iniciar una partida de Conecta 4 (implementación R pura)
#'
#' @description Implementación de referencia del bucle de juego íntegramente en
#'   R. Es funcionalmente equivalente a \code{\link{iniciar_partida}} pero usa
#'   el motor minimax escrito en R (\code{\link{minimax}}) en lugar del motor
#'   C++ compilado. Es más lenta que la versión por defecto, pero el código es
#'   directamente legible en R y resulta útil para entender el algoritmo o para
#'   depuración.
#'
#'   Permite además guardar el árbol de búsqueda completo mediante
#'   \code{guardar_arbol = TRUE} y después analizarlo con
#'   \code{\link{encontrar_mejor_variante}} y
#'   \code{\link{mostrar_mejor_variante}}.
#'
#' @param profundidad Profundidad de búsqueda del algoritmo minimax.
#'   Por defecto 5.
#' @param turno Jugador que comienza: 1 para humano, 2 para IA.
#'   Por defecto 1.
#' @param profAdaptative Lógico. Si \code{TRUE} ajusta la profundidad
#'   dinámicamente según las columnas disponibles. Por defecto \code{TRUE}.
#' @param auto Lógico. Si \code{TRUE} la IA juega contra sí misma sin
#'   intervención humana. Por defecto \code{FALSE}.
#' @param guardar_arbol Lógico. Si \code{TRUE} el minimax construye el árbol
#'   completo de búsqueda, accesible a través del objeto devuelto.
#'   Por defecto \code{FALSE}.
#'
#' @return La matriz 6×7 con el estado final del tablero (invisible).
#'
#' @examples
#' \dontrun{
#' # Partida humano vs IA con motor R
#' iniciar_partida_r(profundidad = 5)
#'
#' # IA contra sí misma guardando el árbol de búsqueda
#' tablero <- iniciar_partida_r(auto = TRUE, profundidad = 5, guardar_arbol = TRUE)
#' }
#'
#' @seealso \code{\link{iniciar_partida}}, \code{\link{minimax}},
#'   \code{\link{encontrar_mejor_variante}}
#'
#' @export
iniciar_partida_r <- function(profundidad = 5, turno = 1,
                               profAdaptative = TRUE, auto = FALSE,
                               guardar_arbol = FALSE) {

  resultado <- "DRAW"
  tablero <- reiniciar_tablero()
  p <- visualizar_tablero(tablero)
  print(p)
  i <- 1
  j <- 0

  if (!auto) {
    if (turno == 2) {
      mejor_jugada_IA <- minimax(tablero, profundidad, maximizandoIA = TRUE,
                                 guardar_arbol = guardar_arbol)
      tablero <- realizar_jugada(tablero, mejor_jugada_IA$jugada, 2)
      p <- visualizar_tablero(tablero)
      print(p)
      cat(sprintf("valoracion IA: %d\n", evaluar_posicion(tablero)))
      cat(sprintf("jugada realizada: %d\n", mejor_jugada_IA$jugada))

      if (juego_terminado(tablero)$finalizado) {
        cat("gana IA\n")
        return(invisible(tablero))
      }
      i <- i + 1
      j <- 1
    }

    while (i <= (42 - j)) {

      tablero <- turno_humano(tablero)
      p <- visualizar_tablero(tablero)
      print(p)
      cat(sprintf("valoracion HU: %d\n", evaluar_posicion(tablero)))

      if (juego_terminado(tablero)$finalizado) {
        resultado <- juego_terminado(tablero)$resultado
        break
      }
      i <- i + 1

      prof <- if (profAdaptative) .adaptativa(tablero, profundidad_base = profundidad) else profundidad

      tik <- system.time({
        mejor_jugada_IA <- minimax(tablero, prof, maximizandoIA = TRUE,
                                   guardar_arbol = guardar_arbol)
      })

      tablero <- realizar_jugada(tablero, mejor_jugada_IA$jugada, 2)
      p <- visualizar_tablero(tablero)
      print(p)

      cat("\n")
      cat("-----------------------------------------------------------------\n")
      cat(sprintf("valoracion IA:    %d\n",    mejor_jugada_IA$puntuacion))
      cat(sprintf("jugada realizada: %d\n",    mejor_jugada_IA$jugada))
      cat(sprintf("profundidad:      %d\n",    prof))
      n_nodos <- if (!is.null(mejor_jugada_IA$arbol)) length(mejor_jugada_IA$env$arbol@idNodo) else NA
      cat(sprintf("num. nodos:       %s\n",    ifelse(is.na(n_nodos), "NA", n_nodos)))
      cat(sprintf("tiempo:           %.2f s\n", round(tik[[3]], 2)))
      cat(sprintf("nod/s:            %s\n",    ifelse(is.na(n_nodos), "NA", round(n_nodos / tik[[3]], 3))))
      cat("-----------------------------------------------------------------\n\n")

      if (juego_terminado(tablero)$finalizado) {
        resultado <- juego_terminado(tablero)$resultado
        break
      }
      i <- i + 1
    }

    cat(sprintf("Resultado: %s\n", resultado))
  }

  if (auto) {
    while (i <= 42) {
      turno_actual <- ((i - 1) %% 2) + 1

      prof <- if (profAdaptative) .adaptativa(tablero, profundidad_base = profundidad) else profundidad
      maxIA <- (turno_actual == 2)

      tik <- system.time({
        mejor_jugada_IA <- minimax(tablero, prof, maximizandoIA = maxIA,
                                   guardar_arbol = guardar_arbol)
      })

      tablero <- realizar_jugada(tablero, mejor_jugada_IA$jugada, turno_actual)
      p <- visualizar_tablero(tablero)
      print(p)

      cat("\n")
      cat("-----------------------------------------------------------------\n")
      cat(sprintf("valoracion IA:    %d\n",    mejor_jugada_IA$puntuacion))
      cat(sprintf("jugada realizada: %d\n",    mejor_jugada_IA$jugada))
      cat(sprintf("profundidad:      %d\n",    prof))
      n_nodos <- if (!is.null(mejor_jugada_IA$arbol)) length(mejor_jugada_IA$env$arbol@idNodo) else NA
      cat(sprintf("num. nodos:       %s\n",    ifelse(is.na(n_nodos), "NA", n_nodos)))
      cat(sprintf("tiempo:           %.2f s\n", round(tik[[3]], 2)))
      cat(sprintf("nod/s:            %s\n",    ifelse(is.na(n_nodos), "NA", round(n_nodos / tik[[3]], 3))))
      cat("-----------------------------------------------------------------\n\n")

      if (juego_terminado(tablero)$finalizado) {
        resultado <- switch(as.character(juego_terminado(tablero)$resultado),
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
