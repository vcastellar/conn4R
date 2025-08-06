#' evaluación estática de una posición representada en tablero
#'
#' @description evalua una posición mediante criterios estáticos basados en el
#' número de casillas conectadas del jugador en turno
#' @param tablero matriz que representa el estado de un tablero
#' @param turno qué jugador comienza pa partida: 1 para humano, 2 para IA.
#' @examples
#' tablero <- crear_posicion_aleatoria(5)
#' visualizar_tablero(tablero)
#' evaluar_posicion(tablero)
#' .evaluar_turno(tablero, 1)
#' .evaluar_turno(tablero, 2)


# Precomputar en inicialización
generar_coordenadas_lineas <- function() {
  lineas <- list()

  # Horizontales
  for (fila in 1:6) {
    for (col in 1:4) {
      lineas[[length(lineas)+1]] <- cbind(fila, col:(col+3))
    }
  }

  # Verticales
  for (col in 1:7) {
    for (fila in 1:3) {
      lineas[[length(lineas)+1]] <- cbind(fila:(fila+3), col)
    }
  }

  # Diagonal ↘
  for (fila in 1:3) {
    for (col in 1:4) {
      lineas[[length(lineas)+1]] <- cbind(fila:(fila+3), col:(col+3))
    }
  }

  # Diagonal ↙
  for (fila in 1:3) {
    for (col in 4:7) {
      lineas[[length(lineas)+1]] <- cbind(fila:(fila+3), col:(col-3))
    }
  }

  return(lineas)
}

# Guardar globalmente (fuera de la función)
lineas_posibles <- generar_coordenadas_lineas()


bitboards <- matrix(c(
  0.0, 0.5, 1.0, 1.5, 1.0, 0.5, 0.0,
  0.5, 1.0, 1.5, 2.0, 1.5, 1.0, 0.5,
  1.0, 1.5, 2.0, 2.5, 2.0, 1.5, 1.0,
  1.5, 2.0, 2.5, 3.0, 2.5, 2.0, 1.5,
  2.0, 2.5, 3.0, 3.5, 3.0, 2.5, 2.0,
  0.5, 1.0, 1.5, 2.0, 1.5, 1.0, 0.5
), nrow = 6, byrow = TRUE)


# constantes
punt1 <- 1
punt2 <- 10
punt3 <- 100
punt4 <- 100000

# función principal
evaluar_posicion <- function(tablero, turno) {
  turno_ia <- 2
  turno_oponente <- ifelse(turno_ia == 1, 2, 1)
  
  puntBit <- sum(bitboards * (1 * (tablero == turno_ia))) - sum(bitboards * (1 * (tablero == turno_oponente)))
  
  eval_turno <- .evaluar_turno(tablero, turno_ia)
  eval_oponente <- .evaluar_turno(tablero, turno_oponente)
  eval_diff <- eval_turno - eval_oponente
  
  return(eval_diff + puntBit)
}

#función evaluar turno

.evaluar_turno <- function(tablero, turno) {
  puntuacion <- 0
  for (coords in lineas_posibles) {
    linea <- tablero[coords]
    puntuacion <- puntuacion + .evaluar_linea(linea, turno)
  }
  return(puntuacion)
}


# función evaluar linea
.evaluar_linea <- function(linea, turno) {
  n <- sum(linea == turno)
  v <- sum(linea == 0)
  
  if (n == 4) {
    puntuacion <- punt4
  } else if (n == 3 && v == 1) {
    puntuacion <- punt3
  } else if (n == 2 && v == 2) {
    puntuacion <- punt2
  } else if (n == 1 && v == 3) {
    puntuacion <- punt1
  } else {
    puntuacion <- 0
  }
  
  # if (turno == 1) puntuacion <- -puntuacion
  
  return(puntuacion)
}

