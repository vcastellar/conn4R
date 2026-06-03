#' Algoritmo minimax con poda alpha-beta, tabla de transposición y árbol opcional
#'
#' @description Implementa el algoritmo minimax con poda alpha-beta para decidir
#'   la mejor jugada dado un estado del tablero. Incorpora una tabla de transposición
#'   que evita re-evaluar posiciones ya visitadas, y permite activar opcionalmente
#'   la construcción del árbol de búsqueda completo para análisis.
#'
#' @param tablero Matriz 6x7 que representa el estado del tablero. Celdas: 0 vacío,
#'   1 humano, 2 IA.
#' @param profundidad Entero. Profundidad máxima del árbol de búsqueda.
#' @param maximizandoIA Lógico. \code{TRUE} indica nodo MAX (turno de la IA);
#'   \code{FALSE} indica nodo MIN (turno del humano).
#' @param alpha Cota inferior de la ventana alpha-beta. Por defecto \code{-Inf}.
#' @param beta Cota superior de la ventana alpha-beta. Por defecto \code{+Inf}.
#' @param guardar_arbol Lógico. Si \code{TRUE} construye y devuelve el árbol S4
#'   completo de búsqueda en \code{$arbol}. Por defecto \code{FALSE} para evitar
#'   el coste O(n²) de los appends a slots S4 durante la partida normal.
#'
#' @return Lista con los siguientes elementos:
#' \describe{
#'   \item{puntuacion}{Puntuación minimax de la posición resultante.}
#'   \item{jugada}{Columna elegida por la IA (entero 1-7).}
#'   \item{env}{Entorno interno con la tabla de transposición (\code{env$tt})
#'     y, si \code{guardar_arbol = TRUE}, el árbol S4 (\code{env$arbol}).}
#'   \item{arbol}{Objeto \code{\link{arbol}} con todos los nodos evaluados, o
#'     \code{NULL} si \code{guardar_arbol = FALSE}.}
#' }
#'
#' @details
#' \strong{Poda alpha-beta:} reduce el espacio de búsqueda descartando ramas que
#' no pueden mejorar la evaluación ya conocida. A profundidad 5 con 3 movimientos
#' realizados: 4.677 nodos con poda frente a 19.607 sin poda.
#'
#' \strong{Tabla de transposición:} al inicio de cada nodo se consulta si la
#' posición ya fue evaluada a profundidad suficiente. Si la cota almacenada
#' (EXACT, LOWER o UPPER) es compatible con la ventana actual se retorna
#' directamente sin explorar el subárbol. La mejor jugada almacenada también
#' se usa para mejorar el orden de exploración en visitas posteriores.
#'
#' \strong{Ordenación de jugadas:} las victorias inmediatas se evalúan primero,
#' seguidas de los bloqueos de victoria rival, y finalmente el resto ordenado
#' por evaluación estática. Esto maximiza la eficacia de la poda alpha-beta.
#'
#' @examples
#' tablero <- crear_posicion_aleatoria(7)
#' visualizar_tablero(tablero)
#'
#' # Uso normal durante la partida (sin árbol, más rápido)
#' system.time({
#'   kk <- minimax(tablero = tablero, profundidad = 7, maximizandoIA = TRUE)
#' })
#' kk$puntuacion
#' kk$jugada
#'
#' # Con árbol completo para análisis
#' kk_analisis <- minimax(tablero, profundidad = 5, maximizandoIA = TRUE,
#'                        guardar_arbol = TRUE)
#' max(kk_analisis$env$arbol@idNodo)
#' kk_analisis$arbol


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
