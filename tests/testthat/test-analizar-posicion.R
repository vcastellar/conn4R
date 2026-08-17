test_that("analizar_posicion recomienda bloquear una victoria inmediata", {
  # Tres fichas humanas alineadas en la base: la IA debe bloquear en la 4.
  tablero <- matrix(0L, nrow = 6, ncol = 7)
  tablero[6, 1:3] <- 1L
  tablero[6, 5:6] <- 2L

  analisis <- analizar_posicion(tablero, turno = 2, profundidad = 5)

  expect_s3_class(analisis, "analisis_conn4R")
  expect_equal(analisis$jugada, 4L)
  expect_equal(analisis$turno, 2L)
  expect_equal(analisis$profundidad, 5L)
})

test_that("analizar_posicion infiere el turno del recuento de fichas", {
  # 2 fichas humanas y 1 de la IA -> le toca a la IA (jugador 2).
  tablero <- crear_posicion(c(4, 4, 3))
  expect_equal(sum(tablero == 1L), 2L)
  expect_equal(sum(tablero == 2L), 1L)

  analisis <- analizar_posicion(tablero, profundidad = 4)
  expect_equal(analisis$turno, 2L)

  # Posición equilibrada en fichas -> le toca al humano (jugador 1).
  tablero2 <- crear_posicion(c(4, 4))
  expect_equal(analizar_posicion(tablero2, profundidad = 4)$turno, 1L)
})

test_that("analizar_posicion detecta un mate forzado a favor de la IA", {
  tablero <- matrix(0L, nrow = 6, ncol = 7)
  tablero[4:6, 3] <- 2L  # tres en vertical de la IA, gana en la 3

  analisis <- analizar_posicion(tablero, turno = 2, profundidad = 1)
  expect_equal(analisis$jugada, 3L)
  expect_gt(analisis$puntuacion, 1e9)
  expect_match(analisis$veredicto, "IA")
})

test_that("analizar_posicion valida sus argumentos", {
  tablero <- reiniciar_tablero()

  expect_error(analizar_posicion(tablero, turno = 3), "turno")
  expect_error(analizar_posicion(tablero, profundidad = -1), "profundidad")
  expect_error(analizar_posicion(tablero, profundidad = 2.5), "profundidad")
  expect_error(analizar_posicion(matrix(0L, 5, 7)), "matriz 6x7")

  # Un tablero con más fichas de la IA que del humano no es humano-primero
  # legal, así que el turno no puede inferirse.
  ilegal <- matrix(0L, nrow = 6, ncol = 7)
  ilegal[6, 1:2] <- 2L
  expect_error(analizar_posicion(ilegal), "inferir el turno")
})

test_that("print.analisis_conn4R devuelve invisiblemente su entrada", {
  tablero <- crear_posicion(c(4, 4, 3))
  analisis <- analizar_posicion(tablero, profundidad = 4)

  salida <- capture.output(resultado <- withVisible(print(analisis)))
  expect_false(resultado$visible)
  expect_identical(resultado$value, analisis)
  expect_true(any(grepl("Jugada recomendada", salida)))
  expect_true(any(grepl("Variante principal", salida)))
})
