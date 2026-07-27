test_that("una victoria humana inmediata se bloquea a cualquier profundidad", {
  tablero <- matrix(0L, nrow = 6, ncol = 7)
  tablero[6, 1:3] <- 1L
  tablero[6, 5:6] <- 2L

  for (profundidad in c(2L, 5L)) {
    resultado <- minimax(tablero, profundidad, maximizandoIA = TRUE)
    expect_equal(resultado$jugada, 4L)
  }
})

test_that("la búsqueda de quiescencia ve una victoria justo tras el horizonte", {
  tablero <- matrix(0L, nrow = 6, ncol = 7)
  tablero[6, 1:3] <- 1L
  tablero[6, 5:6] <- 2L

  # Aunque no queden plies normales, el turno humano gana en la columna 4.
  resultado <- minimax(tablero, profundidad = 0L, maximizandoIA = FALSE)
  expect_equal(resultado$jugada, 4L)
  expect_lt(resultado$puntuacion, -1e9)

  # Desde el ply anterior, la IA debe impedir ese mate fuera del horizonte.
  expect_equal(minimax(tablero, profundidad = 1L, maximizandoIA = TRUE)$jugada, 4L)
})

test_that("una posición terminal puntúa por resultado y no por heurística", {
  tablero <- matrix(0L, nrow = 6, ncol = 7)
  tablero[6, 1:3] <- 1L

  resultado <- minimax(tablero, 2L, maximizandoIA = TRUE)
  expect_equal(resultado$jugada, 4L)

  sin_bloqueo <- realizar_jugada(tablero, 5L, 2L)
  victoria_humana <- realizar_jugada(sin_bloqueo, 4L, 1L)
  expect_true(juego_terminado(victoria_humana)$finalizado)
  expect_equal(juego_terminado(victoria_humana)$resultado, 1L)
})

test_that("la posición aleatoria reproducible conserva el bloqueo en búsquedas profundas", {
  set.seed(123)
  tablero <- crear_posicion_aleatoria(profundidad = 11)

  expect_equal(sum(tablero == 1L), 6L)
  expect_equal(sum(tablero == 2L), 5L)
  expect_equal(minimax(tablero, 5L, TRUE)$jugada, 3L)
  expect_equal(minimax(tablero, 10L, TRUE)$jugada, 3L)
})
