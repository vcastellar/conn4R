#' profundidad adaptativa
#' @examples
#' tablero <- crear_posicion_aleatoria(10)
#' .adaptativa(tablero)


# .adaptativa <- function(numJugadas, profundidad, alpha = 1.5) {
#   
#   if (alpha < 1) {
#     stop("parámetro alpha debe ser  a >= 1")
#   }
#   
#   factor <- (1 / (numJugadas / 7))^(1/alpha)
#   profs  <- round(profundidad * factor)
#   prof   <- min(profs, round(6 * numJugadas))
#   
#   return(prof)
# }

# Cálculo de número de columnas disponibles (ramas posibles)
.columnasValidas <- function(tablero) {
  sum(apply(tablero, 2, function(x) {sum(x == 0) / 6}))
}


.adaptativa <- function(tablero = tablero, profundidad_base = 7, profundidad_max = 15) {
  b <- .columnasValidas(tablero)
  if (b <= 1) return(profundidad_max)

  # Presupuesto fijo de nodos equivalente a b_max^profundidad_base
  N <- 7 ^ profundidad_base
  # Nunca bajar de profundidad_base — cuando el tablero está abierto (b grande) mantenemos
  # la profundidad mínima; cuando se cierra (b pequeño) la aumentamos hasta profundidad_max
  d_range <- profundidad_base:profundidad_max
  nodos_estimados <- b ^ d_range
  d_mejor <- d_range[which.min(abs(nodos_estimados - N))]

  return(d_mejor)
}


# .adaptativa <- Vectorize(.adaptativa, vectorize.args = "numJugadas")



# .numJugadas <- function(tablero) {
#   sum(apply(tablero, 2, function(x) {sum(x == 0) / 6}))
# }

.numJugadas <- function(tablero) {
  sum(tablero != 0)
}


