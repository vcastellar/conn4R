test_that("crear_posicion reproduce una secuencia de jugadas alternando jugadores", {
  tablero <- crear_posicion(c(4, 4, 3, 5))

  # Humano abre: columnas 4 y 3 son del humano; 4 y 5 de la IA.
  expect_equal(tablero[6, 4], 1L)  # 1a jugada, humano
  expect_equal(tablero[5, 4], 2L)  # 2a jugada, IA, apilada sobre la anterior
  expect_equal(tablero[6, 3], 1L)  # 3a jugada, humano
  expect_equal(tablero[6, 5], 2L)  # 4a jugada, IA
  expect_equal(sum(tablero == 1L), 2L)
  expect_equal(sum(tablero == 2L), 2L)
})

test_that("crear_posicion respeta turno_inicial", {
  tablero <- crear_posicion(c(4, 3), turno_inicial = 2)
  expect_equal(tablero[6, 4], 2L)
  expect_equal(tablero[6, 3], 1L)
})

test_that("crear_posicion con jugadas vacías devuelve un tablero vacío", {
  expect_equal(crear_posicion(), reiniciar_tablero())
  expect_equal(crear_posicion(integer()), reiniciar_tablero())
})

test_that("crear_posicion valida sus argumentos y las jugadas", {
  expect_error(crear_posicion(c(4, 4), turno_inicial = 3), "turno_inicial")
  expect_error(crear_posicion(c(4, 8)), "columnas enteras")
  expect_error(crear_posicion(c(4, 4.5)), "columnas enteras")

  # Columna llena tras seis fichas en la misma columna.
  expect_error(
    crear_posicion(rep(4, 7)),
    "columna 4 está llena"
  )
})

test_that("crear_posicion se detiene si la partida ya ha terminado", {
  # Humano abre: coloca tres fichas verticales en la columna 1 (jugadas 1,3,5)
  # mientras la IA responde en la 2 (jugadas 2,4). La 7a jugada del humano en
  # la columna 1 completaría cuatro en raya; una jugada posterior es ilegal.
  expect_error(
    crear_posicion(c(1, 2, 1, 2, 1, 2, 1, 2)),
    "ya había terminado"
  )
})
