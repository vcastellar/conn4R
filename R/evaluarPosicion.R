#' Evaluación estática de una posición
#'
#' @description Evalúa una posición del tablero mediante criterios estáticos.
#'   El resultado es siempre desde la perspectiva de la IA (jugador 2):
#'   valores positivos son favorables para la IA, negativos para el humano.
#'
#'   La puntuación combina cinco componentes:
#'   \enumerate{
#'     \item \strong{Líneas abiertas}: solo se puntúan líneas de 4 que no
#'       contengan piezas del oponente. Esquema de puntos: 1 pieza = 1,
#'       2 piezas = 10, 3 piezas = 1.000, 4 piezas = 100.000.
#'     \item \strong{Control de centro}: bonus posicional mediante la matriz
#'       \code{bitboards} (valores 0-7, máximo en el centro del tablero).
#'     \item \strong{Amenazas dobles}: bonus/penalización de 10.000 cuando un
#'       jugador tiene dos o más amenazas abiertas simultáneas (trampa inevitable).
#'     \item \strong{Paridad de amenazas}: en Conecta 4, las amenazas en filas
#'       impares (1,3,5 desde abajo) favorecen al jugador 1 y las de filas pares
#'       (2,4,6) al jugador 2. Se pondera cada amenaza según su paridad.
#'     \item \strong{Amenazas apiladas}: dos amenazas en la misma columna crean
#'       una trampa inevitable independientemente de quién mueva.
#'     \item \strong{Conectividad}: bonus por piezas propias adyacentes (horizontal,
#'       vertical, diagonal) que potencian futuras líneas.
#'   }
#'
#' @param tablero Matriz 6x7 que representa el estado del tablero. Celdas:
#'   0 vacío, 1 humano, 2 IA.
#'
#' @return Entero. Puntuación de la posición desde la perspectiva de la IA.
#'
#' @examples
#' tablero <- crear_posicion_aleatoria(15)
#' visualizar_tablero(tablero)
#' evaluar_posicion(tablero)
#'
#' # Evaluación individual por jugador (función interna)
#' conn4R:::.evaluar_turno(tablero, 1)
#' conn4R:::.evaluar_turno(tablero, 2)


#' @export
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

#' @export
generar_indices_posibles <- function(lineas_posibles) {
  indices <- sapply(lineas_posibles, function(coords) {
    (coords[,1]) + (coords[,2] - 1) * 6
  })

  return(indices)
}

indices_posibles <- generar_indices_posibles(lineas_posibles)


bitboards <- matrix(c(
  0L, 1L, 2L, 3L, 2L, 1L, 0L,
  1L, 2L, 3L, 4L, 3L, 2L, 1L,
  2L, 3L, 4L, 5L, 4L, 3L, 2L,
  3L, 4L, 5L, 6L, 5L, 4L, 3L,
  4L, 5L, 6L, 7L, 6L, 5L, 4L,
  1L, 2L, 3L, 4L, 3L, 2L, 1L
), nrow = 6, byrow = TRUE)


# constantes
punt1 <- 1L
punt2 <- 10L
punt3 <- 1000L
punt4 <- 100000L

# Bonus de paridad: amenazas en filas impares (1,3,5 desde abajo = filas 6,4,2 en matriz)
# favorecen al jugador 1; filas pares (2,4,6 desde abajo = filas 5,3,1) al jugador 2.
# La fila de la matriz está invertida: fila 1 = arriba, fila 6 = abajo.
# Desde abajo: fila_desde_abajo = 7 - fila_matriz
# Par desde abajo (fila_matriz impar) => favorece IA (jugador 2)
# Impar desde abajo (fila_matriz par) => favorece humano (jugador 1)
BONUS_PARIDAD <- 300L   # peso del bonus de paridad por amenaza

# Bonus por amenazas apiladas (dos en la misma columna) — trampa inevitable
BONUS_APILADA <- 8000L

# Bonus de conectividad por pieza adyacente propia
BONUS_CONEXION <- 3L

# Cuenta amenazas abiertas con información de paridad de fila.
# Retorna lista con: count (total), pares (fila par desde abajo), impares (fila impar desde abajo)
.contar_amenazas_detalle <- function(tablero, turno) {
  count   <- 0L
  pares   <- 0L   # amenazas en filas pares desde abajo (favorecen jugador 2 / IA)
  impares <- 0L   # amenazas en filas impares desde abajo (favorecen jugador 1)
  cols_amenaza <- integer(0)  # columnas con amenaza, para detectar apiladas

  for (coords in lineas_posibles) {
    linea <- tablero[coords]
    if (sum(linea == turno) == 3L && sum(linea == 0L) == 1L) {
      empty_pos <- which(linea == 0L)
      fila_mat  <- coords[empty_pos, 1]
      col       <- coords[empty_pos, 2]
      # Celda accesible si está en la fila inferior o la celda de abajo está ocupada
      if (fila_mat == 6L || tablero[fila_mat + 1L, col] != 0L) {
        count <- count + 1L
        cols_amenaza <- c(cols_amenaza, col)
        fila_desde_abajo <- 7L - fila_mat  # 1..6, 6 = fila más baja
        if (fila_desde_abajo %% 2L == 0L) {
          pares <- pares + 1L
        } else {
          impares <- impares + 1L
        }
      }
    }
  }

  # Amenazas apiladas: más de una amenaza en la misma columna
  apiladas <- if (length(cols_amenaza) > 1L) {
    sum(tabulate(cols_amenaza) >= 2L)
  } else {
    0L
  }

  list(count = count, pares = pares, impares = impares, apiladas = apiladas)
}

# Conectividad: suma de adyacentes propios (4-vecindad + diagonales) para cada pieza
.bonus_conectividad <- function(tablero, turno) {
  total <- 0L
  filas_idx <- which(tablero == turno, arr.ind = TRUE)
  if (nrow(filas_idx) == 0L) return(0L)
  deltas <- list(c(0L,1L), c(1L,0L), c(1L,1L), c(1L,-1L))
  for (i in seq_len(nrow(filas_idx))) {
    f <- filas_idx[i, 1L]
    c <- filas_idx[i, 2L]
    for (d in deltas) {
      nf <- f + d[1L]; nc <- c + d[2L]
      if (nf >= 1L && nf <= 6L && nc >= 1L && nc <= 7L && tablero[nf, nc] == turno) {
        total <- total + BONUS_CONEXION
      }
    }
  }
  total
}

# función principal — evalúa siempre desde la perspectiva de la IA (jugador 2)
# positivo = bueno para IA, negativo = bueno para humano
#' @export
evaluar_posicion <- function(tablero) {
  puntBit   <- sum(bitboards * (tablero == 2L)) - sum(bitboards * (tablero == 1L))
  eval_diff <- .evaluar_turno(tablero, 2L) - .evaluar_turno(tablero, 1L)

  # Detalles de amenazas para ambos jugadores
  am_ia  <- .contar_amenazas_detalle(tablero, 2L)
  am_hum <- .contar_amenazas_detalle(tablero, 1L)

  # Bonus/penalización por amenazas dobles (dos amenazas simultáneas = trampa inevitable)
  bonus_doble   <- (am_ia$count  >= 2L) * punt3 * 10L
  penalty_doble <- (am_hum$count >= 2L) * punt3 * 10L

  # Paridad: amenazas en filas pares (desde abajo) son favorables para la IA
  # amenazas en filas impares (desde abajo) son favorables para el humano
  bonus_paridad <- (am_ia$pares  - am_ia$impares)  *  BONUS_PARIDAD -
                   (am_hum$pares - am_hum$impares)  *  BONUS_PARIDAD

  # Amenazas apiladas (misma columna) — prácticamente victoria garantizada
  bonus_apilada   <- am_ia$apiladas  * BONUS_APILADA
  penalty_apilada <- am_hum$apiladas * BONUS_APILADA

  # Conectividad
  conex <- .bonus_conectividad(tablero, 2L) - .bonus_conectividad(tablero, 1L)

  return(
    eval_diff +
    puntBit +
    bonus_doble   - penalty_doble +
    bonus_paridad +
    bonus_apilada - penalty_apilada +
    conex
  )
}

# función evaluar turno — vectorizada sobre las 69 líneas usando indices_posibles (4x69)
# Solo puntúa líneas sin piezas del oponente (líneas "abiertas")
.evaluar_turno <- function(tablero, turno) {
  oponente   <- ifelse(turno == 1L, 2L, 1L)
  tab_vec    <- as.integer(tablero)
  lineas_mat <- matrix(tab_vec[indices_posibles], nrow = 4L)  # 4 x 69

  n <- colSums(lineas_mat == turno)
  v <- colSums(lineas_mat == 0L)
  b <- colSums(lineas_mat == oponente)   # piezas del oponente en la línea

  # Solo líneas sin piezas del oponente (b == 0)
  abierta <- b == 0L
  sum(
    abierta * (
      (n == 4L)              * punt4 +
      (n == 3L & v == 1L)   * punt3 +
      (n == 2L & v == 2L)   * punt2 +
      (n == 1L & v == 3L)   * punt1
    )
  )
}



# función evaluar linea
.evaluar_linea <- function(linea, turno) {
  n <- sum(linea == turno)
  v <- sum(linea == 0)

  if (n == 4) {
    return(punt4)
  } else if (n == 3 && v == 1) {
    return(punt3)
  } else if (n == 2 && v == 2) {
    return(punt2)
  } else if (n == 1 && v == 3) {
    return(punt1)
  } else {
    return(0)
  }

}


