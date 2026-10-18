test_that(".column_from_npc translates a click into the clicked column", {
  lim <- conn4R:::.board_limits
  to_npc <- function(value, limits) (value - limits[[1]]) / diff(limits)

  for (column in 1:7) {
    for (offset in c(-0.45, 0, 0.45)) {
      x <- to_npc(column + offset, lim$x)
      expect_equal(conn4R:::.column_from_npc(x, to_npc(1, lim$y)), column)
      expect_equal(conn4R:::.column_from_npc(x, to_npc(6.9, lim$y)), column)
    }
  }
})

test_that(".column_from_npc returns NA outside the board", {
  lim <- conn4R:::.board_limits
  to_npc <- function(value, limits) (value - limits[[1]]) / diff(limits)

  expect_true(is.na(conn4R:::.column_from_npc(0, 0.5)))
  expect_true(is.na(conn4R:::.column_from_npc(1, 0.5)))
  expect_true(is.na(conn4R:::.column_from_npc(to_npc(4, lim$x), 0)))
})

test_that("human_turn applies the given move without asking for clicks", {
  board <- human_turn(reset_board(), move = 4)
  expect_equal(board[6, 4], 1L)
  expect_equal(sum(board != 0L), 1L)
})

test_that(".viewport_names finds the panel of the drawn board", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(visualize_board(reset_board()))

  vp_names <- conn4R:::.viewport_names(grid::current.vpTree())
  expect_true(any(grepl("^panel\\.", vp_names)))
})
