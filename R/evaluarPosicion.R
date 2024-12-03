#' evaluación estática de una posición representada en tablero
#'
#' @description evalua una posición mediante criterios estáticos basados en el
#' número de casillas conectadas del jugador en turno
#' @param tablero matriz que representa el estado de un tablero
#' @param turno qué jugador comienza pa partida: 1 para humano, 2 para IA.
#' @examples
#' tablero <- crear_posicion_aleatoria(18)
#' visualizar_tablero(tablero)
#' evaluar_posicion(tablero)
#' .evaluar_turno(tablero, 1)
#' .evaluar_turno(tablero, 2)


bitboards <- matrix(0, nrow = 6, ncol = 7)
bitboards[, c(2, 6)] <- 0.5
bitboards[, c(3, 5)] <- 1
bitboards[, 4]       <- 1.5
bitboards[1, ] <- bitboards[1, ] + 0
bitboards[2, ] <- bitboards[2, ] + 0.5
bitboards[3, ] <- bitboards[3, ] + 1
bitboards[4, ] <- bitboards[4, ] + 1.5
bitboards[5, ] <- bitboards[5, ] + 2
bitboards[6, ] <- bitboards[6, ] + 0.5

#' bitboarts
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
# [1,]  0.0  0.5  1.0  1.5  1.0  0.5  0.0
# [2,]  0.5  1.0  1.5  2.0  1.5  1.0  0.5
# [3,]  1.0  1.5  2.0  2.5  2.0  1.5  1.0
# [4,]  1.5  2.0  2.5  3.0  2.5  2.0  1.5
# [5,]  2.0  2.5  3.0  3.5  3.0  2.5  2.0
# [6,]  0.5  1.0  1.5  2.0  1.5  1.0  0.5


evaluar_posicion <- function(tablero) {
  puntBit <- sum(bitboards * (1 * (tablero == 2))) - sum(bitboards * (1 * (tablero == 1)))
  return(.evaluar_turno(tablero, 1) + .evaluar_turno(tablero, 2) + puntBit)
}


.evaluar_turno <- function(tablero, turno) {
  puntuacion <- 0

  # Evaluar líneas horizontales
  for (fila in 1:6) {
    for (columna in 1:4) {
      linea <- tablero[fila, columna:(columna + 3)]
      puntuacion <- puntuacion + .evaluar_linea(linea, turno)
    }
  }

  # Evaluar líneas verticales
  for (columna in 1:7) {
    for (fila in 1:3) {
      linea <- tablero[fila:(fila + 3), columna]
      puntuacion <- puntuacion + .evaluar_linea(linea, turno)
    }
  }

  # Evaluar líneas diagonales (de izquierda a derecha)
  for (fila in 1:3) {
    for (columna in 1:4) {
      linea <- c(tablero[fila, columna], tablero[fila + 1, columna + 1],
                 tablero[fila + 2, columna + 2], tablero[fila + 3, columna + 3])
      puntuacion <- puntuacion + .evaluar_linea(linea, turno)
    }
  }

  # Evaluar líneas diagonales (de derecha a izquierda)
  for (fila in 1:3) {
    for (columna in 4:7) {
      linea <- c(tablero[fila, columna], tablero[fila + 1, columna - 1],
                 tablero[fila + 2, columna - 2], tablero[fila + 3, columna - 3])
      puntuacion <- puntuacion + .evaluar_linea(linea, turno)
    }
  }

  return(puntuacion)
}

# prompt: crea la función evaluar_linea necesaria para que funcióne la función anterior

.evaluar_linea <- function(linea, turno) {
  puntuacion <- 0

  if (all(linea == turno)) {
    puntuacion <- 100000 # Línea completa del jugador actual

  } else if (sum(linea == turno) == 3 && sum(linea == 0) == 1) {
    puntuacion <- 100 # Tres fichas en línea con una casilla vacía

  } else if (sum(linea == turno) == 2 && sum(linea == 0) == 2) {
    puntuacion <- 10 # Dos fichas en línea con dos casillas vacías

  } else if (sum(linea == turno) == 1 && sum(linea == 0) == 3) {
    puntuacion <- 1 # Una ficha en línea con tres casillas vacías
  }

  puntuacion <- (-1)^(turno) * puntuacion

  return(puntuacion)
}
