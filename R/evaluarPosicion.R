#' Generar todas las coordenadas de líneas posibles del tablero
#'
#' @description Genera la lista completa de las 69 líneas de 4 celdas
#'   contiguas que existen en un tablero de Conecta 4 (6×7): 24 horizontales,
#'   21 verticales, 12 diagonales ascendentes y 12 descendentes.
#'
#'   El resultado se guarda en el objeto global \code{lineas_posibles} al
#'   cargar el paquete para evitar su recálculo en cada evaluación.
#'
#' @return Lista de 69 matrices de 4×2, donde cada fila es la coordenada
#'   \code{[fila, columna]} (1-based) de una celda de la línea.
#'
#' @examples
#' lineas <- generar_coordenadas_lineas()
#' length(lineas)   # 69
#' lineas[[1]]      # primera línea horizontal (fila 1, columnas 1-4)
#'
#' @export
generar_coordenadas_lineas <- function() {
  lineas <- list()

  # Horizontales
  for (fila in 1:6) {
    for (col in 1:4) {
      lineas[[length(lineas) + 1]] <- cbind(fila, col:(col + 3))
    }
  }

  # Verticales
  for (col in 1:7) {
    for (fila in 1:3) {
      lineas[[length(lineas) + 1]] <- cbind(fila:(fila + 3), col)
    }
  }

  # Diagonal descendente ↘
  for (fila in 1:3) {
    for (col in 1:4) {
      lineas[[length(lineas) + 1]] <- cbind(fila:(fila + 3), col:(col + 3))
    }
  }

  # Diagonal ascendente ↙
  for (fila in 1:3) {
    for (col in 4:7) {
      lineas[[length(lineas) + 1]] <- cbind(fila:(fila + 3), col:(col - 3))
    }
  }

  return(lineas)
}

# Precalculado al cargar el paquete
lineas_posibles <- generar_coordenadas_lineas()

#' Generar índices planos de las líneas posibles
#'
#' @description Convierte la lista de coordenadas matriciales devuelta por
#'   \code{\link{generar_coordenadas_lineas}} en una matriz de índices planos
#'   (column-major) para acceso vectorizado al tablero. El resultado se guarda
#'   en \code{indices_posibles} (4×69) y es usado internamente por
#'   \code{.evaluar_turno()}.
#'
#' @param lineas_posibles Lista de 69 matrices de coordenadas, tal como
#'   devuelve \code{\link{generar_coordenadas_lineas}}.
#'
#' @return Matriz entera 4×69. Cada columna contiene los 4 índices planos
#'   (1-based, column-major) de las celdas que forman una línea.
#'
#' @examples
#' lp <- generar_coordenadas_lineas()
#' idx <- generar_indices_posibles(lp)
#' dim(idx)   # 4 69
#'
#' @export
generar_indices_posibles <- function(lineas_posibles) {
  indices <- sapply(lineas_posibles, function(coords) {
    (coords[, 1]) + (coords[, 2] - 1) * 6
  })
  return(indices)
}

indices_posibles <- generar_indices_posibles(lineas_posibles)

# Matriz de peso posicional: valor máximo en el centro (7), mínimo en esquinas (0).
# Fila 1 = parte superior, fila 6 = parte inferior.
bitboards <- matrix(c(
  0L, 1L, 2L, 3L, 2L, 1L, 0L,
  1L, 2L, 3L, 4L, 3L, 2L, 1L,
  2L, 3L, 4L, 5L, 4L, 3L, 2L,
  3L, 4L, 5L, 6L, 5L, 4L, 3L,
  4L, 5L, 6L, 7L, 6L, 5L, 4L,
  1L, 2L, 3L, 4L, 3L, 2L, 1L
), nrow = 6, byrow = TRUE)

# Esquema de puntos por número de piezas propias en una línea abierta
punt1 <- 1L
punt2 <- 10L
punt3 <- 1000L
punt4 <- 100000L

# Peso del bonus de paridad por amenaza.
# Amenazas en filas pares desde abajo (fila_matriz impar) favorecen a la IA (jugador 2).
# Amenazas en filas impares desde abajo (fila_matriz par) favorecen al humano (jugador 1).
BONUS_PARIDAD <- 300L

# Bonus por dos amenazas apiladas en la misma columna — trampa inevitable
BONUS_APILADA <- 8000L

# Bonus por pieza propia adyacente (horizontales, verticales y diagonales)
BONUS_CONEXION <- 3L

# Cuenta amenazas abiertas (3 piezas propias + 1 celda vacía accesible) con info de paridad
.contar_amenazas_detalle <- function(tablero, turno) {
  count   <- 0L
  pares   <- 0L
  impares <- 0L
  cols_amenaza <- integer(0)

  for (coords in lineas_posibles) {
    linea <- tablero[coords]
    if (sum(linea == turno) == 3L && sum(linea == 0L) == 1L) {
      empty_pos        <- which(linea == 0L)
      fila_mat         <- coords[empty_pos, 1]
      col              <- coords[empty_pos, 2]
      accesible        <- fila_mat == 6L || tablero[fila_mat + 1L, col] != 0L
      if (!accesible) next

      count        <- count + 1L
      cols_amenaza <- c(cols_amenaza, col)
      fila_abajo   <- 7L - fila_mat  # 1..6 desde abajo
      if (fila_abajo %% 2L == 0L) {
        pares   <- pares   + 1L
      } else {
        impares <- impares + 1L
      }
    }
  }

  apiladas <- if (length(cols_amenaza) > 1L) {
    sum(tabulate(cols_amenaza) >= 2L)
  } else {
    0L
  }

  list(count = count, pares = pares, impares = impares, apiladas = apiladas)
}

# Bonus de conectividad: piezas propias adyacentes (4 direcciones desde cada celda)
.bonus_conectividad <- function(tablero, turno) {
  total    <- 0L
  filas_idx <- which(tablero == turno, arr.ind = TRUE)
  if (nrow(filas_idx) == 0L) return(0L)
  deltas <- list(c(0L, 1L), c(1L, 0L), c(1L, 1L), c(1L, -1L))
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

#' Evaluación estática de una posición (implementación R)
#'
#' @description Evalúa una posición del tablero combinando seis componentes
#'   heurísticos. El resultado siempre se expresa desde la perspectiva de la
#'   IA (jugador 2): valores positivos indican ventaja para la IA y valores
#'   negativos ventaja para el humano.
#'
#'   \strong{Componentes de la puntuación:}
#'   \enumerate{
#'     \item \strong{Líneas abiertas}: solo se puntúan líneas de 4 sin piezas
#'       del oponente. Esquema: 1 pieza = 1, 2 = 10, 3 = 1 000, 4 = 100 000.
#'     \item \strong{Control de centro}: la matriz de pesos posicionales
#'       \code{bitboards} (0-7) premia las casillas centrales.
#'     \item \strong{Amenazas dobles}: bonus/penalización de 10 000 cuando un
#'       jugador tiene dos o más amenazas simultáneas (trampa inevitable).
#'     \item \strong{Paridad de amenazas}: las amenazas en filas pares desde
#'       abajo favorecen a la IA; las de filas impares al humano. Cada amenaza
#'       contribuye ±300 según su fila.
#'     \item \strong{Amenazas apiladas}: dos amenazas en la misma columna
#'       valen un bonus de 8 000 (trampa que no puede bloquearse).
#'     \item \strong{Conectividad}: 3 puntos por cada pareja de piezas propias
#'       adyacentes (mejora posiciones futuras).
#'   }
#'
#' @param tablero Matriz 6×7 con el estado del tablero. Celdas: 0 vacío,
#'   1 humano, 2 IA.
#'
#' @return Entero. Puntuación de la posición desde la perspectiva de la IA.
#'   Positivo = ventaja IA, negativo = ventaja humano.
#'
#' @examples
#' set.seed(42)
#' tablero <- crear_posicion_aleatoria(15)
#' visualizar_tablero(tablero)
#' evaluar_posicion(tablero)
#'
#' # Desglose por jugador (función interna)
#' conn4R:::.evaluar_turno(tablero, 1)
#' conn4R:::.evaluar_turno(tablero, 2)
#'
#' @seealso \code{\link{evaluar_posicion_cpp}} (versión C++ equivalente)
#'
#' @export
evaluar_posicion <- function(tablero) {
  puntBit   <- sum(bitboards * (tablero == 2L)) - sum(bitboards * (tablero == 1L))
  eval_diff <- .evaluar_turno(tablero, 2L) - .evaluar_turno(tablero, 1L)

  am_ia  <- .contar_amenazas_detalle(tablero, 2L)
  am_hum <- .contar_amenazas_detalle(tablero, 1L)

  bonus_doble   <- (am_ia$count  >= 2L) * punt3 * 10L
  penalty_doble <- (am_hum$count >= 2L) * punt3 * 10L

  bonus_paridad <- (am_ia$pares  - am_ia$impares)  * BONUS_PARIDAD -
                   (am_hum$pares - am_hum$impares)  * BONUS_PARIDAD

  bonus_apilada   <- am_ia$apiladas  * BONUS_APILADA
  penalty_apilada <- am_hum$apiladas * BONUS_APILADA

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

# Evaluación vectorizada de líneas abiertas de un jugador sobre las 69 líneas
.evaluar_turno <- function(tablero, turno) {
  oponente   <- ifelse(turno == 1L, 2L, 1L)
  tab_vec    <- as.integer(tablero)
  lineas_mat <- matrix(tab_vec[indices_posibles], nrow = 4L)  # 4 x 69

  n <- colSums(lineas_mat == turno)
  v <- colSums(lineas_mat == 0L)
  b <- colSums(lineas_mat == oponente)

  abierta <- b == 0L
  sum(
    abierta * (
      (n == 4L)            * punt4 +
      (n == 3L & v == 1L) * punt3 +
      (n == 2L & v == 2L) * punt2 +
      (n == 1L & v == 3L) * punt1
    )
  )
}

# Evaluación escalar de una línea individual de 4 celdas
.evaluar_linea <- function(linea, turno) {
  n <- sum(linea == turno)
  v <- sum(linea == 0)

  if      (n == 4)             punt4
  else if (n == 3 && v == 1)  punt3
  else if (n == 2 && v == 2)  punt2
  else if (n == 1 && v == 3)  punt1
  else                         0
}
