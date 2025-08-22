#' @examples
#' tablero <- crear_posicion_aleatoria(12)
#' visualizar_tablero(tablero)
#' system.time({
#'   kk <- minimax(tablero = tablero, profundidad = 7, maximizandoIA = FALSE)
#' })
#' max(kk$env$arbol@idNodo)
#' kk$puntuacion
#' kk$jugada
#' bestVar <- encontrar_mejor_variante(kk$env$arbol)
#' mostrar_mejor_variante(tablero, bestVar, pausa = 3000)


minimax_iterative <- function(tablero, 
                              profundidad_maxima = 6, 
                              maximizandoIA = TRUE, 
                              tiempo_limite = 5) {
  
  start_time <- Sys.time()
  mejor_jugada_global <- NA
  mejor_puntuacion_global <- if (maximizandoIA) -Inf else Inf
  env_global <- NULL
  
  for (d in 1:profundidad_maxima) {
    tiempo_actual <- as.numeric(Sys.time() - start_time, units = "secs")
    if (tiempo_actual > tiempo_limite) break
    
    # Nueva búsqueda con profundidad d
    resultado <- minimax(tablero,
                         profundidad = d,
                         maximizandoIA = maximizandoIA,
                         alpha = -Inf,
                         beta = Inf,
                         env = NULL,         # reiniciar árbol en cada iteración
                         idPadre = NA_integer_)
    
    # Actualizar mejor jugada
    if (!is.na(resultado$jugada)) {
      mejor_jugada_global <- resultado$jugada
      mejor_puntuacion_global <- resultado$puntuacion
      env_global <- resultado$env
    }
    
    cat("- Profundidad", d, "- Jugada:", mejor_jugada_global,
        "- Puntuación:", mejor_puntuacion_global,
        "- Tiempo:", round(tiempo_actual, 2), "seg\n")
  }
  
  return(list(
    jugada = mejor_jugada_global,
    puntuacion = mejor_puntuacion_global,
    env = env_global
  ))
}
