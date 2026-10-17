test_that(".columna_desde_npc traduce un clic en la columna pulsada", {
  lim <- conn4R:::.limites_tablero
  a_npc <- function(valor, rango) (valor - rango[[1]]) / diff(rango)

  for (columna in 1:7) {
    for (desplazamiento in c(-0.45, 0, 0.45)) {
      x <- a_npc(columna + desplazamiento, lim$x)
      expect_equal(conn4R:::.columna_desde_npc(x, a_npc(1, lim$y)), columna)
      expect_equal(conn4R:::.columna_desde_npc(x, a_npc(6.9, lim$y)), columna)
    }
  }
})

test_that(".columna_desde_npc devuelve NA fuera del tablero", {
  lim <- conn4R:::.limites_tablero
  a_npc <- function(valor, rango) (valor - rango[[1]]) / diff(rango)

  expect_true(is.na(conn4R:::.columna_desde_npc(0, 0.5)))
  expect_true(is.na(conn4R:::.columna_desde_npc(1, 0.5)))
  expect_true(is.na(conn4R:::.columna_desde_npc(a_npc(4, lim$x), 0)))
})

test_that("turno_humano aplica la jugada indicada sin pedir clics", {
  tablero <- turno_humano(reiniciar_tablero(), jugada = 4)
  expect_equal(tablero[6, 4], 1L)
  expect_equal(sum(tablero != 0L), 1L)
})

test_that(".nombres_viewports encuentra el panel del tablero dibujado", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(visualizar_tablero(reiniciar_tablero()))

  nombres <- conn4R:::.nombres_viewports(grid::current.vpTree())
  expect_true(any(grepl("^panel\\.", nombres)))
})
