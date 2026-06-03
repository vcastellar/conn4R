#' algoritmo minimax con poda alpha-beta y tabla de transposición
#'
#' @description función que mediante un algoritmo mini-max la IA decide cuál es su mejor
#' jugada dada una cierta posición del tablero
#'
#' @param tablero a matrix representing the state of the game board
#' @param profundidad un entero que fija la profundidad del árbol de jugadas a analizar
#' @param maximizandoIA Booleano. TRUE significa que se maximiza la puntuación de la IA
#'   FALSE se minimiza la puntuación del jugador humano
#' @param alpha parámetro de la poda alpha-beta. Por defecto -Inf
#' @param beta parámetro de la poda alpha-beta. Por defecto +Inf
#'
#' @return returns a list with the following contents
#' \itemize{
#' \item{puntuacion}: puntuación obtenida al evaluar la posición al realizar la 'jugada'
#' \item{jugada}: jugada elegida por el algoritmo
#' \item{arbol}: objeto de clase \code{arbol} con todos los nodos realmente analizados
#' }
#' @details
#' la poda alpha-beta reduce drásticamente el número de nodos que se evaluan:
#' a profundidad 5, con tres movimientos realizados en el tablero, el algoritmo minimax calcula:
#' \itemize{
#' \item con poda alpha-beta: 4.677 nodos
#' \item sin poda alpha-beta: 19.607 nodos
#' }
#' @examples
#' tablero <- crear_posicion_aleatoria(7)
#' visualizar_tablero(tablero)
#' system.time({
#'   kk <- minimax(tablero = tablero, profundidad = 7, maximizandoIA = TRUE)
#' })
#' max(kk$env$arbol@idNodo)
#' kk$puntuacion
#' kk$jugada
#' kk$env$arbol


minimax <- function(tablero, profundidad, maximizandoIA, .maxProf = profundidad,
                    alpha = -Inf, beta = Inf, env = NULL,
                    idPadre = NA_integer_, guardar_arbol = FALSE) {

  if (is.null(env)) {
    env <- new.env()
    env$tt           <- nueva_tt()
    env$guardar_arbol <- guardar_arbol

    if (guardar_arbol) {
      env$arbol <- new("arbol")
      env$arbol <- actualizar(env$arbol,
                              idPadre     = NA_integer_,
                              turno       = maximizandoIA,
                              jugada      = NA_integer_,
                              profundidad = 0L,
                              puntuacion  = evaluar_posicion(tablero))
      idPadre <- tail(env$arbol@idNodo, 1)
    }
  }

  turno <- ifelse(maximizandoIA, 2L, 1L)

  # Caso base
  if (profundidad == 0L || juego_terminado(tablero)$finalizado) {
    return(list(
      puntuacion = evaluar_posicion(tablero),
      jugada     = NA,
      env        = env
    ))
  }

  # ── Consulta tabla de transposición ──────────────────────────────────────
  clave   <- .tt_clave(tablero)
  tt_hit  <- .tt_lookup(env$tt, clave, profundidad, alpha, beta)
  if (!is.null(tt_hit)) {
    return(list(
      puntuacion = tt_hit$punt,
      jugada     = tt_hit$jugada,
      env        = env
    ))
  }

  # Guardar ventana original para clasificar la cota al final
  alpha_orig <- alpha
  beta_orig  <- beta

  mejor_puntuacion <- if (maximizandoIA) -Inf else Inf
  mejor_jugada     <- NA
  comparar         <- if (maximizandoIA) `>` else `<`

  # Ordenar jugadas; si la TT tiene una mejor jugada, ponerla primero
  jugadas_df <- ordenar_jugadas(tablero, turno)
  jugadas_candidatas <- jugadas_df$jugadas

  tt_jugada <- env$tt[[clave]]$jugada
  if (!is.null(tt_jugada) && !is.na(tt_jugada)) {
    jugadas_candidatas <- c(tt_jugada,
                            jugadas_candidatas[jugadas_candidatas != tt_jugada])
  }

  for (columna in jugadas_candidatas) {
    nuevo_tablero <- realizar_jugada(tablero, columna, turno)

    if (env$guardar_arbol) {
      env$arbol <- actualizar(env$arbol,
                              idPadre     = idPadre,
                              turno       = !maximizandoIA,
                              jugada      = as.integer(columna),
                              profundidad = .maxProf - as.integer(profundidad) + 1L,
                              puntuacion  = NA_real_)
      nuevo_id <- tail(env$arbol@idNodo, 1)
    }

    res <- minimax(nuevo_tablero, profundidad - 1L, !maximizandoIA,
                   .maxProf = .maxProf, alpha, beta, env,
                   idPadre = if (env$guardar_arbol) nuevo_id else NA_integer_)

    if (env$guardar_arbol) {
      idx <- which(env$arbol@idNodo == nuevo_id)
      if (length(idx) == 1L) {
        env$arbol@puntuacion[idx] <- res$puntuacion
      }
    }

    if (comparar(res$puntuacion, mejor_puntuacion)) {
      mejor_puntuacion <- res$puntuacion
      mejor_jugada     <- columna
    }

    if (maximizandoIA) {
      alpha <- max(alpha, mejor_puntuacion)
    } else {
      beta  <- min(beta,  mejor_puntuacion)
    }

    if (beta <= alpha) break
  }

  # ── Almacenar en tabla de transposición ──────────────────────────────────
  .tt_store(env$tt, clave, profundidad, mejor_puntuacion,
            alpha_orig, beta_orig, mejor_jugada)

  return(list(
    puntuacion = mejor_puntuacion,
    jugada     = mejor_jugada,
    env        = env,
    arbol      = if (env$guardar_arbol) env$arbol else NULL
  ))
}
