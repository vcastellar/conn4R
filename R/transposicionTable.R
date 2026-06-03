#' Tabla de transposición para minimax con alpha-beta
#'
#' @description Crea y gestiona una tabla de transposición (TT) que almacena
#'   resultados de posiciones ya evaluadas, evitando recalcular nodos que se
#'   alcanzan por distintas secuencias de movimientos (transposiciones).
#'
#'   Cada entrada contiene:
#'   \describe{
#'     \item{prof}{Profundidad restante con la que se evaluó el nodo. Solo se
#'       usa una entrada si \code{prof >= profundidad} requerida.}
#'     \item{punt}{Puntuación minimax obtenida.}
#'     \item{flag}{Tipo de cota: \code{TT_EXACT} (0) valor exacto;
#'       \code{TT_LOWER} (1) cota inferior — el nodo causó un corte beta;
#'       \code{TT_UPPER} (2) cota superior — el nodo no mejoró alpha.}
#'     \item{jugada}{Mejor jugada encontrada. Se antepone a la lista de
#'       candidatas en visitas posteriores para mejorar la ordenación.}
#'   }
#'
#' @return \code{nueva_tt} devuelve un entorno con hash activado listo para
#'   usarse como tabla de transposición.


# Flags de la tabla de transposición
TT_EXACT <- 0L
TT_LOWER <- 1L   # lower bound (fallo alto / corte beta)
TT_UPPER <- 2L   # upper bound (fallo bajo / nodo ALL)

nueva_tt <- function() {
  new.env(hash = TRUE, parent = emptyenv())
}

# Genera la clave de la posición: codificación compacta del tablero 6x7
.tt_clave <- function(tablero) {
  paste(as.integer(tablero), collapse = "")
}

# Consulta la TT. Devuelve la entrada si es utilizable, NULL en caso contrario.
.tt_lookup <- function(tt, clave, profundidad, alpha, beta) {
  if (!exists(clave, envir = tt, inherits = FALSE)) return(NULL)
  e <- tt[[clave]]
  # Solo es válida si fue evaluada a profundidad igual o mayor
  if (e$prof < profundidad) return(NULL)

  if (e$flag == TT_EXACT)                       return(e)
  if (e$flag == TT_LOWER && e$punt >= beta)     return(e)
  if (e$flag == TT_UPPER && e$punt <= alpha)    return(e)
  NULL
}

# Almacena un resultado en la TT.
# alpha_orig / beta_orig: ventana ANTES de que el nodo la modificase.
.tt_store <- function(tt, clave, profundidad, puntuacion, alpha_orig, beta_orig, jugada) {
  flag <- if      (puntuacion >= beta_orig)  TT_LOWER
          else if (puntuacion <= alpha_orig) TT_UPPER
          else                               TT_EXACT

  # Reemplazar solo si la nueva entrada es de mayor o igual profundidad
  if (!exists(clave, envir = tt, inherits = FALSE) ||
      tt[[clave]]$prof <= profundidad) {
    tt[[clave]] <- list(prof = profundidad, punt = puntuacion,
                        flag = flag, jugada = jugada)
  }
}
