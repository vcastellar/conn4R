#' evaluación estática de una posición representada en tablero
#'
#' @description evalua una posición mediante criterios estáticos basados en el
#' número de casillas conectadas del jugador en turno
#' @param tablero matriz que representa el estado de un tablero
#' @param turno qué jugador comienza pa partida: 1 para humano, 2 para IA.
#' @examples
#' tablero <- crear_posicion_aleatoria(15)
#' visualizar_tablero(tablero)
#' -----------------------------------------------------------------------------
#' evaluación estática
#' -----------------------------------------------------------------------------
#' evaluar_posicion(tablero)
#' .evaluar_turno(tablero, 1)
#' .evaluar_turno(tablero, 2)
#' -----------------------------------------------------------------------------
#' 
#'  -----------------------------------------------------------------------------
#' evaluación dinámica
#' -----------------------------------------------------------------------------
#' kk <- minimax(tablero, 7, TRUE)
#' kk$puntuacion
#' -----------------------------------------------------------------------------


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

  # Diagonal
  for (fila in 1:3) {
    for (col in 1:4) {
      lineas[[length(lineas)+1]] <- cbind(fila:(fila+3), col:(col+3))
    }
  }

  # Diagonal
  for (fila in 1:3) {
    for (col in 4:7) {
      lineas[[length(lineas)+1]] <- cbind(fila:(fila+3), col:(col-3))
    }
  }

  return(lineas)
}

# Guardar globalmente (fuera de la función)
lineas_posibles <- generar_coordenadas_lineas()

generar_indices_posibles <- function(lineas_posibles) {
  indices <- sapply(lineas_posibles, function(coords) {
    (coords[,1]) + (coords[,2] - 1) * 6
  })
  
  return(indices)
}

indices_posibles <- generar_indices_posibles(lineas_posibles)


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
punt3 <- 1000
punt4 <- 100000

# Cuenta amenazas abiertas: líneas con 3 piezas propias + 1 celda vacía accesible por gravedad
.contar_amenazas_abiertas <- function(tablero, turno) {
  count <- 0L
  for (coords in lineas_posibles) {
    linea <- tablero[coords]
    if (sum(linea == turno) == 3L && sum(linea == 0L) == 1L) {
      empty_pos <- which(linea == 0L)
      fila <- coords[empty_pos, 1]
      col  <- coords[empty_pos, 2]
      # Celda accesible si está en la fila inferior o la celda de abajo está ocupada
      if (fila == 6L || tablero[fila + 1L, col] != 0L) {
        count <- count + 1L
      }
    }
  }
  count
}

# función principal — evalúa siempre desde la perspectiva de la IA (jugador 2)
# positivo = bueno para IA, negativo = bueno para humano
evaluar_posicion <- function(tablero) {
  puntBit   <- sum(bitboards * (tablero == 2L)) - sum(bitboards * (tablero == 1L))
  eval_diff <- .evaluar_turno(tablero, 2L) - .evaluar_turno(tablero, 1L)

  # Bonus/penalización por amenazas dobles (dos amenazas simultáneas = trampa inevitable)
  amenazas_ia  <- .contar_amenazas_abiertas(tablero, 2L)
  amenazas_hum <- .contar_amenazas_abiertas(tablero, 1L)
  bonus_doble   <- (amenazas_ia  >= 2L) * punt3 * 10L
  penalty_doble <- (amenazas_hum >= 2L) * punt3 * 10L

  return(eval_diff + puntBit + bonus_doble - penalty_doble)
}

# función evaluar turno — vectorizada sobre las 69 líneas usando indices_posibles (4x69)
.evaluar_turno <- function(tablero, turno) {
  tab_vec    <- as.integer(tablero)
  lineas_mat <- matrix(tab_vec[indices_posibles], nrow = 4L)  # 4 x 69

  n <- colSums(lineas_mat == turno)
  v <- colSums(lineas_mat == 0L)

  sum(
    (n == 4L)              * punt4 +
    (n == 3L & v == 1L)   * punt3 +
    (n == 2L & v == 2L)   * punt2 +
    (n == 1L & v == 3L)   * punt1
  )
}



