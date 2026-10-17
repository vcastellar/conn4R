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

capas_elemento <- function(grafico, elemento) {
  Filter(function(capa) {
    is.data.frame(capa$data) && identical(unique(capa$data$elemento), elemento)
  }, grafico$layers)
}

test_that("visualizar_tablero destaca únicamente la última jugada indicada", {
  tablero <- reiniciar_tablero()
  tablero <- realizar_jugada(tablero, 4L, 1L)

  grafico <- visualizar_tablero(tablero, c(6L, 4L))
  capas <- capas_elemento(grafico, "ultima_jugada")
  expect_length(capas, 1L)
  expect_equal(unique(capas[[1]]$data$cx), 4L)
  expect_equal(unique(capas[[1]]$data$cy), 1L)
  expect_equal(capas[[1]]$aes_params$colour, "#FFFFFF")

  tablero <- realizar_jugada(tablero, 5L, 2L)
  grafico <- visualizar_tablero(tablero, c(6L, 5L))
  capas <- capas_elemento(grafico, "ultima_jugada")
  expect_length(capas, 1L)
  expect_equal(unique(capas[[1]]$data$cx), 5L)

  expect_length(capas_elemento(visualizar_tablero(tablero), "ultima_jugada"), 0L)
})

test_that("visualizar_tablero resalta la línea ganadora y el estado", {
  tablero <- crear_posicion(c(4, 3, 4, 3, 4, 3, 4))

  grafico <- visualizar_tablero(tablero)
  capas <- capas_elemento(grafico, "linea_ganadora")
  expect_length(capas, 1L)
  expect_setequal(unique(paste(capas[[1]]$data$cx, capas[[1]]$data$cy)),
                  paste(4, 1:4))
  expect_equal(grafico$labels$subtitle, "Victoria del humano")

  expect_equal(visualizar_tablero(reiniciar_tablero())$labels$subtitle,
               "0 jugadas")
  expect_length(capas_elemento(visualizar_tablero(reiniciar_tablero()),
                               "linea_ganadora"), 0L)
})

test_that("visualizar_tablero puede dibujarse", {
  tablero <- crear_posicion(c(4, 3, 4, 3, 4, 3, 4))
  archivo <- tempfile(fileext = ".png")
  on.exit(unlink(archivo))
  grDevices::png(archivo)
  on.exit(grDevices::dev.off(), add = TRUE, after = FALSE)
  expect_no_error(print(visualizar_tablero(tablero, c(3L, 4L))))
  expect_no_error(print(visualizar_tablero(reiniciar_tablero())))
})

test_that("visualizar_tablero valida las coordenadas de la última jugada", {
  tablero <- reiniciar_tablero()

  expect_error(visualizar_tablero(tablero, c(6L, 4L)), "señalar una ficha")
  expect_error(visualizar_tablero(tablero, c(7L, 4L)), "señalar una ficha")
})
