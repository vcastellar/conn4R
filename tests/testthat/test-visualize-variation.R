test_that("visualize_variation alternates players and keeps the initial position", {
  board <- reset_board()
  board[6, 1] <- 2L

  result <- visualize_variation(
    board,
    turn = 1L,
    variation = c(4L, 4L, 3L),
    delay = 0
  )

  expect_equal(result[6, 1], 2L)
  expect_equal(result[6, 4], 1L)
  expect_equal(result[5, 4], 2L)
  expect_equal(result[6, 3], 1L)
})

test_that("visualize_variation accepts an empty variation", {
  board <- reset_board()

  expect_equal(
    visualize_variation(board, turn = 2L, variation = integer(), delay = 0),
    board
  )
})

test_that("visualize_variation validates its arguments and moves", {
  board <- reset_board()

  expect_error(visualize_variation(board, 3, 4, 0), "turn")
  expect_error(visualize_variation(board, 1, 8, 0), "variation")
  expect_error(visualize_variation(board, 1, 4, -1), "delay")

  board[, 4] <- rep(c(1L, 2L), 3L)
  expect_error(
    visualize_variation(board, 1, 4, 0),
    "column 4 is full"
  )
})

element_layers <- function(chart, element) {
  Filter(function(layer) {
    is.data.frame(layer$data) && identical(unique(layer$data$element), element)
  }, chart$layers)
}

test_that("visualize_board highlights only the given last move", {
  board <- reset_board()
  board <- make_move(board, 4L, 1L)

  chart <- visualize_board(board, c(6L, 4L))
  layers <- element_layers(chart, "last_move")
  expect_length(layers, 1L)
  expect_equal(unique(layers[[1]]$data$cx), 4L)
  expect_equal(unique(layers[[1]]$data$cy), 1L)
  expect_equal(layers[[1]]$aes_params$colour, "#FFFFFF")

  board <- make_move(board, 5L, 2L)
  chart <- visualize_board(board, c(6L, 5L))
  layers <- element_layers(chart, "last_move")
  expect_length(layers, 1L)
  expect_equal(unique(layers[[1]]$data$cx), 5L)

  expect_length(element_layers(visualize_board(board), "last_move"), 0L)
})

test_that("visualize_board highlights the winning line and the game state", {
  board <- create_position(c(4, 3, 4, 3, 4, 3, 4))

  chart <- visualize_board(board)
  layers <- element_layers(chart, "winning_line")
  expect_length(layers, 1L)
  expect_setequal(unique(paste(layers[[1]]$data$cx, layers[[1]]$data$cy)),
                  paste(4, 1:4))
  expect_equal(chart$labels$subtitle, "Human wins")

  expect_equal(visualize_board(reset_board())$labels$subtitle,
               "0 moves")
  expect_length(element_layers(visualize_board(reset_board()),
                               "winning_line"), 0L)
})

test_that("visualize_board can be drawn", {
  board <- create_position(c(4, 3, 4, 3, 4, 3, 4))
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path))
  grDevices::png(path)
  on.exit(grDevices::dev.off(), add = TRUE, after = FALSE)
  expect_no_error(print(visualize_board(board, c(3L, 4L))))
  expect_no_error(print(visualize_board(reset_board())))
})

test_that("visualize_board validates the coordinates of the last move", {
  board <- reset_board()

  expect_error(visualize_board(board, c(6L, 4L)), "point to a piece")
  expect_error(visualize_board(board, c(7L, 4L)), "point to a piece")
})
