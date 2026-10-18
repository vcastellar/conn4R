test_that("create_position replays a sequence of moves alternating players", {
  board <- create_position(c(4, 4, 3, 5))

  # The human opens: columns 4 and 3 are the human's; 4 and 5 the AI's.
  expect_equal(board[6, 4], 1L)  # 1st move, human
  expect_equal(board[5, 4], 2L)  # 2nd move, AI, stacked on the previous one
  expect_equal(board[6, 3], 1L)  # 3rd move, human
  expect_equal(board[6, 5], 2L)  # 4th move, AI
  expect_equal(sum(board == 1L), 2L)
  expect_equal(sum(board == 2L), 2L)
})

test_that("create_position honors first_player", {
  board <- create_position(c(4, 3), first_player = 2)
  expect_equal(board[6, 4], 2L)
  expect_equal(board[6, 3], 1L)
})

test_that("create_position with no moves returns an empty board", {
  expect_equal(create_position(), reset_board())
  expect_equal(create_position(integer()), reset_board())
})

test_that("create_position validates its arguments and the moves", {
  expect_error(create_position(c(4, 4), first_player = 3), "first_player")
  expect_error(create_position(c(4, 8)), "integer columns")
  expect_error(create_position(c(4, 4.5)), "integer columns")

  # Full column after six pieces in the same column.
  expect_error(
    create_position(rep(4, 7)),
    "column 4 is full"
  )
})

test_that("create_position stops if the game is already over", {
  # The human opens: three vertical pieces in column 1 (moves 1,3,5)
  # while the AI replies in column 2 (moves 2,4). The 7th move, by the human
  # in column 1, completes four in a row; any later move is illegal.
  expect_error(
    create_position(c(1, 2, 1, 2, 1, 2, 1, 2)),
    "already over"
  )
})
