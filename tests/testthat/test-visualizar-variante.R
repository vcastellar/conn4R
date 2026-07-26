test_that("visualizar_variante alterna jugadores y conserva la posición inicial", {
  tablero <- reiniciar_tablero()
  tablero[6, 1] <- 2L

  resultado <- visualizar_variante(
    tablero,
    turno = 1L,
    variante = c(4L, 4L, 3L),
    lapso = 0
  )

  expect_equal(resultado[6, 1], 2L)
  expect_equal(resultado[6, 4], 1L)
  expect_equal(resultado[5, 4], 2L)
  expect_equal(resultado[6, 3], 1L)
})

test_that("visualizar_variante acepta una variante vacía", {
  tablero <- reiniciar_tablero()

  expect_equal(
    visualizar_variante(tablero, turno = 2L, variante = integer(), lapso = 0),
    tablero
  )
})

test_that("visualizar_variante valida sus argumentos y jugadas", {
  tablero <- reiniciar_tablero()

  expect_error(visualizar_variante(tablero, 3, 4, 0), "turno")
  expect_error(visualizar_variante(tablero, 1, 8, 0), "variante")
  expect_error(visualizar_variante(tablero, 1, 4, -1), "lapso")

  tablero[, 4] <- rep(c(1L, 2L), 3L)
  expect_error(
    visualizar_variante(tablero, 1, 4, 0),
    "columna 4 está llena"
  )
})
