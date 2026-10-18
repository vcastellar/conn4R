test_that("an immediate human win is blocked at any depth", {
  board <- matrix(0L, nrow = 6, ncol = 7)
  board[6, 1:3] <- 1L
  board[6, 5:6] <- 2L

  for (depth in c(2L, 5L)) {
    result <- minimax(board, depth, maximizingAI = TRUE)
    expect_equal(result$move, 4L)
  }
})

test_that("quiescence search sees a win right after the horizon", {
  board <- matrix(0L, nrow = 6, ncol = 7)
  board[6, 1:3] <- 1L
  board[6, 5:6] <- 2L

  # Even with no normal plies left, the human to move wins in column 4.
  result <- minimax(board, depth = 0L, maximizingAI = FALSE)
  expect_equal(result$move, 4L)
  expect_lt(result$score, -1e9)
  expect_equal(result$normal_nodes, 1)
  expect_equal(result$tactical_nodes, 1)
  expect_equal(result$nodes,
               result$normal_nodes + result$tactical_nodes)

  # From the previous ply, the AI must prevent that mate beyond the horizon.
  expect_equal(minimax(board, depth = 1L, maximizingAI = TRUE)$move, 4L)
})

test_that("double threats keep the mate-in-two distance", {
  human_threat <- matrix(0L, nrow = 6, ncol = 7)
  human_threat[6, ] <- c(0L, 1L, 1L, 1L, 0L, 1L, 1L)

  result <- minimax(human_threat, 0L, maximizingAI = TRUE)
  # 35 empty cells remain after the defense and the human win.
  expect_equal(result$score, -1000000035)
  expect_true(is.na(result$move))
  expect_equal(result$tactical_nodes, 2)

  ai_threat <- matrix(0L, nrow = 6, ncol = 7)
  ai_threat[6, ] <- c(0L, 2L, 2L, 2L, 0L, 2L, 2L)

  result <- minimax(ai_threat, 0L, maximizingAI = FALSE)
  expect_equal(result$score, 1000000035)
  expect_true(is.na(result$move))
  expect_equal(result$tactical_nodes, 2)
})

test_that("quiescence detects vertical wins for both players", {
  board <- matrix(0L, nrow = 6, ncol = 7)
  board[4:6, 3] <- 2L

  ai_win <- minimax(board, 0L, maximizingAI = TRUE)
  expect_equal(ai_win$move, 3L)
  expect_gt(ai_win$score, 1e9)

  board[,] <- 0L
  board[4:6, 6] <- 1L

  human_win <- minimax(board, 0L, maximizingAI = FALSE)
  expect_equal(human_win$move, 6L)
  expect_lt(human_win$score, -1e9)
})

test_that("a terminal position is scored by its result, not by the heuristic", {
  board <- matrix(0L, nrow = 6, ncol = 7)
  board[6, 1:3] <- 1L

  result <- minimax(board, 2L, maximizingAI = TRUE)
  expect_equal(result$move, 4L)

  no_block <- make_move(board, 5L, 2L)
  human_win <- make_move(no_block, 4L, 1L)
  expect_true(game_over(human_win)$finished)
  expect_equal(game_over(human_win)$result, 1L)
})

test_that("the reproducible random position keeps the block in deep searches", {
  set.seed(123)
  board <- create_random_position(depth = 11)

  expect_equal(sum(board == 1L), 6L)
  expect_equal(sum(board == 2L), 5L)
  expect_equal(minimax(board, 5L, TRUE)$move, 3L)
  expect_equal(minimax(board, 10L, TRUE)$move, 3L)
})

test_that("the C++ engine is loaded and its routines are registered", {
  symbol <- getNativeSymbolInfo("_conn4R_minimax", PACKAGE = "conn4R")

  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_equal(symbol$numParameters, 4L)
})
