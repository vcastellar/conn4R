test_that(".result_label gives the same text for every result code", {
  expect_equal(conn4R:::.result_label(1L), "HUMAN WINS (player 1)")
  expect_equal(conn4R:::.result_label(2L), "AI WINS (player 2)")
  expect_equal(conn4R:::.result_label(0L), "DRAW")
})

test_that("an AI vs AI game ends with a readable result", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  output <- capture.output(board <- start_game(depth = 1, auto = TRUE,
                                               adaptiveDepth = FALSE))
  expect_true(game_over(board)$finished)
  expect_match(tail(output, 1), "^Result: (HUMAN WINS|AI WINS|DRAW)")
})
