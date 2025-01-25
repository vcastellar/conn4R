#' profundidad adaptativa
#' @examples
#' profundidad <- 7
#' numJugadas <- seq(7, 1, by = -1/7)
#' profs <- .adaptativa(numJugadas, profundidad, alpha = 1.5)
#' (numNodosAdap <- numJugadas^profs)
#' (numNodosOrig <- numJugadas^profundidad)
#' ylim = c(0, 1.1 * max(c(numNodosAdap, numNodosOrig)))
#' plot(numJugadas, profs, type = "b")
#' plot(numJugadas, numNodosAdap, type = 'b', pch = 19, ylim = ylim)
#' lines(numJugadas, numNodosOrig, type = 'b', col = "red", pch = 19)



.adaptativa <- function(numJugadas, profundidad, alpha = 1.5) {
  
  if (alpha < 1) {
    stop("parámetro alpha debe ser  a >= 1")
  }
  
  factor <- (1 / (numJugadas / 7))^(1/alpha)
  profs  <- round(profundidad * factor)
  prof   <- min(profs, round(6 * numJugadas))
  
  return(prof)
}


.adaptativa <- Vectorize(.adaptativa, vectorize.args = "numJugadas")



.numJugadas <- function(tablero) {
  sum(apply(tablero, 2, function(x) {sum(x == 0) / 6}))
}


