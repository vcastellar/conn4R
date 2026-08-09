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
  expect_equal(resultado$nodos_normales, 1)
  expect_equal(resultado$nodos_tacticos, 1)
  expect_equal(resultado$nodos,
               resultado$nodos_normales + resultado$nodos_tacticos)

  # Desde el ply anterior, la IA debe impedir ese mate fuera del horizonte.
  expect_equal(minimax(tablero, profundidad = 1L, maximizandoIA = TRUE)$jugada, 4L)
})

test_that("las amenazas dobles conservan la distancia de mate en dos", {
  amenaza_humana <- matrix(0L, nrow = 6, ncol = 7)
  amenaza_humana[6, ] <- c(0L, 1L, 1L, 1L, 0L, 1L, 1L)

  resultado <- minimax(amenaza_humana, 0L, maximizandoIA = TRUE)
  # Quedan 35 huecos después de la defensa y la victoria humana.
  expect_equal(resultado$puntuacion, -1000000035)
  expect_true(is.na(resultado$jugada))
  expect_equal(resultado$nodos_tacticos, 2)

  amenaza_ia <- matrix(0L, nrow = 6, ncol = 7)
  amenaza_ia[6, ] <- c(0L, 2L, 2L, 2L, 0L, 2L, 2L)

  resultado <- minimax(amenaza_ia, 0L, maximizandoIA = FALSE)
  expect_equal(resultado$puntuacion, 1000000035)
  expect_true(is.na(resultado$jugada))
  expect_equal(resultado$nodos_tacticos, 2)
})

test_that("la quiescencia detecta victorias verticales para ambos jugadores", {
  tablero <- matrix(0L, nrow = 6, ncol = 7)
  tablero[4:6, 3] <- 2L

  victoria_ia <- minimax(tablero, 0L, maximizandoIA = TRUE)
  expect_equal(victoria_ia$jugada, 3L)
  expect_gt(victoria_ia$puntuacion, 1e9)

  tablero[,] <- 0L
  tablero[4:6, 6] <- 1L

  victoria_humana <- minimax(tablero, 0L, maximizandoIA = FALSE)
  expect_equal(victoria_humana$jugada, 6L)
  expect_lt(victoria_humana$puntuacion, -1e9)
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

test_that("el motor C++ está cargado y sus rutinas están registradas", {
  simbolo <- getNativeSymbolInfo("_conn4R_minimax", PACKAGE = "conn4R")

  expect_s3_class(simbolo, "NativeSymbolInfo")
  expect_equal(simbolo$numParameters, 3L)
})
