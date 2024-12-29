#' profundidad adaptativa
#' @examples
#' tablero <- crear_posicion_aleatoria(10)
#' numJugadas <- seq(7, 1, by = -1/6)
#' profundidad <- 7
#' profs <- profundidadAdaptativa(numJugadas, profundidad, a = 0.2, p = 0.8)
#' (numNodosAdap <- numJugadas^profs)
#' (numNodosOrig <- numJugadas^profundidad)
#' ylim = c(0, 1.1 * max(c(numNodosAdap, numNodosOrig)))
#' plot(numJugadas, profs, type = "b")
#' plot(numJugadas, numNodosAdap, type = 'b', pch = 19, ylim = ylim)
#' lines(numJugadas, numNodosOrig, type = 'b', col = "red", pch = 19)


.adaptativa <- function(numJugadas, profundidad, a, p) {
  if (!(0 <= p & p <= 1)) {
    stop("parámetro p debe estar en [0,1]")
  }
  if (a > 1) {
    stop("parámetro a debe ser  a <= 1")
  }
  prof <- min(round((profundidad - a) * log(profundidad - a) / log(numJugadas + (1 - p))), 
              round(6 * numJugadas))
  prof <- max(prof, profundidad)
  return(prof)
}

.adaptativa <- Vectorize(.adaptativa, vectorize.args = "numJugadas")


profundidadAdaptativa <- function(numJugadas, profundidad, a = 0.2, p = 0.8) {
    prof = min(.adaptativa(numJugadas, profundidad, a, p), 
               round(6 * numJugadas)
               )
  return(prof)
  
}

profundidadAdaptativa <- Vectorize(profundidadAdaptativa, vectorize.args = "numJugadas")

.numJugadas <- function(tablero) {
  sum(apply(tablero, 2, function(x) {sum(x == 0) / 6}))
}

