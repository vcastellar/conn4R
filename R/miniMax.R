#' algoritmo minimax con poda alpha-beta
#'
#' @description función que mediante un algoritmo mini-max la IA decide cuál es su mejor
#' jugada dada una cierta posición del tablero

#' @param tablero a matrix representing the state of the game board
#' @param profundidad un entero que fija la profundidad del árbol de jugadas a analizar
#' @param maximizandoIA Booleano. TRUE significa que se maximiza la puntuación de la IA
#'   FALSE se minimiza la puntuación del jugador humano
#' @param alpha parámetro de la poda alpha-beta. Por defecto -Inf
#' @param beta parámetro de la poda alpha-beta. Por defecto +Inf

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
                    idPadre = NA_integer_) {

  if (is.null(env)) {
    env <- new.env()
    env$arbol <- new("arbol")

    # Crear nodo raíz: turno = TRUE significa que la IA (MAX) está a punto de jugar.
    env$arbol <- actualizar(env$arbol,
                            idPadre     = NA_integer_,
                            turno       = maximizandoIA,
                            jugada      = NA_integer_,
                            profundidad = 0L,
                            puntuacion  = evaluar_posicion(tablero, 2L))
    idPadre <- tail(env$arbol@idNodo, 1)
  }

  turno <- ifelse(maximizandoIA, 2L, 1L)
  mejor_puntuacion <- if (maximizandoIA) -Inf else Inf
  mejor_jugada <- NA

  # Caso base: profundidad 0 o juego terminado — el nodo ya fue creado por el llamador.
  if (profundidad == 0 || juego_terminado(tablero)$finalizado) {
    return(list(
      puntuacion = evaluar_posicion(tablero, turno),
      jugada     = NA,
      env        = env
    ))
  }

  comparar <- if (maximizandoIA) `>` else `<`

  jugadas_candidatas <- ordenar_jugadas(tablero, turno)$jugadas

  for (columna in jugadas_candidatas) {
    nuevo_tablero <- realizar_jugada(tablero, columna, turno)

    # turno = !maximizandoIA: el nodo hijo es el turno del jugador contrario.
    env$arbol <- actualizar(env$arbol,
                            idPadre     = idPadre,
                            turno       = !maximizandoIA,
                            jugada      = as.integer(columna),
                            profundidad = .maxProf - as.integer(profundidad) + 1L,
                            puntuacion  = NA_real_)

    nuevo_id <- tail(env$arbol@idNodo, 1)

    res <- minimax(nuevo_tablero, profundidad - 1, !maximizandoIA, .maxProf = .maxProf,
                   alpha, beta, env, idPadre = nuevo_id)

    # Etiquetar el nodo con su valor minimax final propagado desde abajo.
    idx <- which(env$arbol@idNodo == nuevo_id)
    if (length(idx) == 1) {
      env$arbol@puntuacion[idx] <- res$puntuacion
    }

    if (comparar(res$puntuacion, mejor_puntuacion)) {
      mejor_puntuacion <- res$puntuacion
      mejor_jugada <- columna
    }

    if (maximizandoIA) {
      alpha <- max(alpha, mejor_puntuacion)
    } else {
      beta <- min(beta, mejor_puntuacion)
    }

    if (beta <= alpha) break
  }

  return(list(
    puntuacion = mejor_puntuacion,
    jugada     = mejor_jugada,
    env        = env,
    arbol      = env$arbol
  ))
}
