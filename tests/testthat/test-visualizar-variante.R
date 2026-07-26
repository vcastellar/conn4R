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

test_that("visualizar_tablero destaca únicamente la última jugada indicada", {
  tablero <- reiniciar_tablero()
  tablero <- realizar_jugada(tablero, 4L, 1L)

  grafico <- visualizar_tablero(tablero, c(6L, 4L))
  expect_equal(nrow(grafico$layers[[2]]$data), 1L)
  expect_equal(grafico$layers[[2]]$data$x, 4L)
  expect_equal(grafico$layers[[2]]$data$y, 1L)
  expect_equal(grafico$layers[[2]]$aes_params$colour, "deepskyblue3")

  tablero <- realizar_jugada(tablero, 5L, 2L)
  grafico <- visualizar_tablero(tablero, c(6L, 5L))
  expect_equal(grafico$layers[[2]]$data$x, 5L)
  expect_equal(nrow(grafico$layers[[2]]$data), 1L)
})

test_that("visualizar_tablero valida las coordenadas de la última jugada", {
  tablero <- reiniciar_tablero()

  expect_error(visualizar_tablero(tablero, c(6L, 4L)), "señalar una ficha")
  expect_error(visualizar_tablero(tablero, c(7L, 4L)), "señalar una ficha")
})
