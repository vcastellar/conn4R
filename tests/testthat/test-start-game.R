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

test_that("each game owns one engine shared by all its moves", {
  calls <- list()
  search <- minimax
  # A short legal game finishes after seven moves, exercising both sides.
  scripted_moves <- c(1L, 2L, 1L, 2L, 1L, 2L, 1L)
  testthat::local_mocked_bindings(
    minimax = function(board, depth, maximizingAI, engine = NULL) {
      calls[[length(calls) + 1L]] <<- engine
      result <- search(board, depth, maximizingAI, engine = engine)
      result$move <- scripted_moves[(length(calls) - 1L) %% 7L + 1L]
      result
    },
    visualize_board = function(...) NULL,
    .package = "conn4R"
  )
  capture.output(first <- start_game(depth = 1, auto = TRUE,
                                     adaptiveDepth = FALSE, memory_mb = 1))
  expect_length(calls, 7)
  expect_true(all(vapply(calls, identical, logical(1), calls[[1]])))
  info <- engine_info(calls[[1]])
  expect_equal(info$searches, 7)
  expect_equal(info$first_player, 1L)
  expect_equal(info$memory_mb, 1)
  expect_equal(game_over(first)$result, 1L)

  capture.output(second <- start_game(depth = 1, turn = 2, auto = TRUE,
                                      adaptiveDepth = FALSE, memory_mb = 2))
  expect_length(calls, 14)
  expect_false(identical(calls[[1]], calls[[8]]))
  expect_true(all(vapply(calls[8:14], identical, logical(1), calls[[8]])))
  expect_equal(engine_info(calls[[8]])$searches, 7)
  expect_equal(engine_info(calls[[8]])$first_player, 2L)
  expect_equal(engine_info(calls[[8]])$memory_mb, 2)
  expect_equal(engine_info(calls[[1]]), info)
  expect_equal(game_over(second)$result, 2L)
})
