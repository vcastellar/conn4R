test_that("analyze_position recommends blocking an immediate win", {
  # Three human pieces in a row on the bottom row: the AI must block in column 4.
  board <- matrix(0L, nrow = 6, ncol = 7)
  board[6, 1:3] <- 1L
  board[6, 5:6] <- 2L

  analysis <- analyze_position(board, turn = 2, depth = 5)

  expect_s3_class(analysis, "conn4R_analysis")
  expect_equal(analysis$move, 4L)
  expect_equal(analysis$turn, 2L)
  expect_equal(analysis$depth, 5L)
})

test_that("analyze_position infers the turn from the piece count", {
  # 2 human pieces and 1 AI piece -> it is the AI's turn (player 2).
  board <- create_position(c(4, 4, 3))
  expect_equal(sum(board == 1L), 2L)
  expect_equal(sum(board == 2L), 1L)

  analysis <- analyze_position(board, depth = 4)
  expect_equal(analysis$turn, 2L)

  # Same number of pieces -> it is the human's turn (player 1).
  board2 <- create_position(c(4, 4))
  expect_equal(analyze_position(board2, depth = 4)$turn, 1L)
})

test_that("analyze_position detects a forced win for the AI", {
  board <- matrix(0L, nrow = 6, ncol = 7)
  board[4:6, 3] <- 2L  # three in a column for the AI, it wins in column 3

  analysis <- analyze_position(board, turn = 2, depth = 1)
  expect_equal(analysis$move, 3L)
  expect_gt(analysis$score, 1e9)
  expect_match(analysis$verdict, "AI")
})

test_that("analyze_position validates its arguments", {
  board <- reset_board()

  expect_error(analyze_position(board, turn = 3), "turn")
  expect_error(analyze_position(board, depth = -1), "depth")
  expect_error(analyze_position(board, depth = 2.5), "depth")
  expect_error(analyze_position(matrix(0L, 5, 7)), "6x7 matrix")

  # A board with more AI pieces than human pieces is not a legal
  # human-first position, so the turn cannot be inferred.
  illegal <- matrix(0L, nrow = 6, ncol = 7)
  illegal[6, 1:2] <- 2L
  expect_error(analyze_position(illegal), "infer the turn")
})

test_that("print.conn4R_analysis returns its input invisibly", {
  board <- create_position(c(4, 4, 3))
  analysis <- analyze_position(board, depth = 4)

  output <- capture.output(result <- withVisible(print(analysis)))
  expect_false(result$visible)
  expect_identical(result$value, analysis)
  expect_true(any(grepl("Recommended move", output)))
  expect_true(any(grepl("Principal variation", output)))
})
