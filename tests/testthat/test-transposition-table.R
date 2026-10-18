test_that("engines have independent, configurable bounded tables", {
  default <- create_engine()
  info <- engine_info(default)
  expect_s3_class(default, "conn4R_engine")
  expect_equal(info$memory_mb, 64)
  expect_equal(info$first_player, 1L)
  expect_lte(info$allocated_bytes, 64 * 1024^2)
  expect_gt(info$allocated_bytes, 63.99 * 1024^2)
  expect_equal(info$entries, 0)
  expect_equal(info$searches, 0)

  smaller <- create_engine(memory_mb = 1, first_player = 2)
  expect_equal(engine_info(smaller)$first_player, 2L)
  expect_lte(engine_info(smaller)$allocated_bytes, 1024^2)
  expect_lt(engine_info(smaller)$capacity, info$capacity)
  minimax(reset_board(), 3L, FALSE, engine = smaller)
  expect_gt(engine_info(smaller)$entries, 0)
  expect_equal(engine_info(default)$entries, 0)
})

test_that("repeat and successor searches reuse results across calls", {
  engine <- create_engine(memory_mb = 2)
  board <- reset_board()
  first <- minimax(board, 4L, FALSE, engine = engine)
  repeated <- minimax(board, 4L, FALSE, engine = engine)
  expect_equal(repeated$score, first$score)
  expect_equal(repeated$move, first$move)
  expect_equal(repeated$variation, first$variation)
  expect_equal(repeated$normal_nodes, 1)
  expect_equal(repeated$tt_hits, 1)
  expect_lt(repeated$nodes, first$nodes)

  child <- make_move(board, first$move, 1L)
  successor <- minimax(child, 3L, TRUE, engine = engine)
  fresh <- minimax(child, 3L, TRUE)
  expect_equal(successor$score, fresh$score)
  expect_equal(successor$normal_nodes, 1)
  expect_equal(successor$tt_hits, 1)
  expect_equal(engine_info(engine)$searches, 3)
})

test_that("deeper entries serve shallow requests but not the reverse", {
  board <- create_position(c(4, 3, 5))
  engine <- create_engine(memory_mb = 2)
  shallow <- minimax(board, 2L, TRUE, engine = engine)
  deep <- minimax(board, 4L, TRUE, engine = engine)
  fresh <- minimax(board, 4L, TRUE)
  expect_gt(deep$normal_nodes, 1)
  expect_equal(deep$score, fresh$score)

  reused <- minimax(board, 2L, TRUE, engine = engine)
  expect_equal(reused$normal_nodes, 1)
  expect_equal(reused$score, deep$score)
  expect_equal(reused$move, deep$move)
  expect_lte(length(reused$variation), 2)

  # Depth zero still invokes tactical stabilization rather than a TT result.
  tactical <- minimax(board, 0L, TRUE, engine = engine)
  expect_equal(tactical$score, minimax(board, 0L, TRUE)$score)
  expect_equal(tactical$tt_hits, 0)
})

test_that("the same board with opposite turns has distinct cached results", {
  board <- create_position(c(1, 5, 2, 6, 3))
  engine <- create_engine(memory_mb = 2)
  ai <- minimax(board, 2L, TRUE, engine = engine)
  human <- minimax(board, 2L, FALSE, engine = engine)
  expect_lt(human$score, -1e9)
  expect_gt(ai$score, human$score)
  expect_equal(human$score, minimax(board, 2L, FALSE)$score)
  expect_equal(ai$score, minimax(board, 2L, TRUE, engine = engine)$score)
  expect_equal(minimax(board, 2L, FALSE, engine = engine)$normal_nodes, 1)
})

test_that("collisions replace entries within budget without changing scores", {
  # One bucket: all positions collide and only four entries fit.
  engine <- create_engine(memory_mb = 0.0002)
  initial <- engine_info(engine)
  expect_equal(initial$capacity, 4)
  boards <- list(reset_board(), create_position(c(4, 3, 5)),
                 create_position(c(1, 5, 2, 6, 3)))
  for (board in boards) {
    for (turn in c(TRUE, FALSE)) {
      tiny <- minimax(board, 3L, turn, engine = engine)
      fresh <- minimax(board, 3L, turn)
      expect_equal(tiny$score, fresh$score)
      expect_true(tiny$move %in% available_moves(board))
    }
  }
  after <- engine_info(engine)
  expect_equal(after$entries, after$capacity)
  expect_equal(after$allocated_bytes, initial$allocated_bytes)
  expect_lte(after$allocated_bytes, 0.0002 * 1024^2)
})

test_that("replacement retains recently accessed entries", {
  engine <- create_engine(memory_mb = 0.0002)
  boards <- lapply(1:5, function(col) make_move(reset_board(), col, 1L))
  for (i in 1:4) minimax(boards[[i]], 1L, TRUE, engine = engine)
  expect_equal(minimax(boards[[1]], 1L, TRUE, engine = engine)$normal_nodes, 1)
  minimax(boards[[5]], 1L, TRUE, engine = engine)
  expect_equal(minimax(boards[[1]], 1L, TRUE, engine = engine)$normal_nodes, 1)
  expect_gt(minimax(boards[[2]], 1L, TRUE, engine = engine)$normal_nodes, 1)
  expect_equal(engine_info(engine)$entries, 4)
})

test_that("clearing resets the cache without reallocating or affecting peers", {
  engine <- create_engine(memory_mb = 1)
  other <- create_engine(memory_mb = 1)
  board <- reset_board()
  first <- minimax(board, 3L, FALSE, engine = engine)
  minimax(board, 3L, FALSE, engine = other)
  before <- engine_info(engine)
  expect_invisible(clear_engine(engine))
  after <- engine_info(engine)
  expect_equal(after$entries, 0)
  expect_equal(after$searches, 0)
  expect_equal(after$hits, 0)
  expect_equal(after$allocated_bytes, before$allocated_bytes)
  expect_gt(engine_info(other)$entries, 0)
  fresh <- minimax(board, 3L, FALSE, engine = engine)
  expect_equal(fresh, first)
})

test_that("principal variations alternate from the actual supplied turn", {
  for (first in 1:2) {
    board <- create_position(c(4, 3, 5), first_player = first)
    turn <- 3L - first
    engine <- create_engine(memory_mb = 2, first_player = first)
    result <- minimax(board, 4L, turn == 2L, engine = engine)
    expect_gte(length(result$variation), 2)
    expect_equal(result$variation[1], result$move)
    for (col in result$variation) {
      expect_false(game_over(board)$finished)
      expect_true(col %in% available_moves(board))
      move <- minimax(board, 1L, turn == 2L, engine = engine)
      expect_equal(move$move, col)
      board <- make_move(board, col, turn)
      turn <- 3L - turn
    }
  }
})

test_that("analyses can share an engine and infer an AI-first turn", {
  engine <- create_engine(memory_mb = 1, first_player = 2)
  board <- create_position(c(4, 3, 5), first_player = 2)
  first <- analyze_position(board, depth = 3, engine = engine)
  second <- analyze_position(board, depth = 3, engine = engine)
  expect_equal(first$turn, 1L)
  expect_equal(second$score, first$score)
  expect_equal(second$nodes, 1)
  expect_equal(second$tt_hits, 1)
  expect_equal(analyze_position(reset_board(), depth = 1, engine = engine)$turn, 2L)
})

test_that("invalid settings and expired or unrelated pointers are rejected", {
  for (memory in c(0, -1, Inf, NA_real_, 1e-10)) {
    expect_error(create_engine(memory_mb = memory), "memory_mb")
  }
  for (player in c(0, 3, 1.5, NA_real_)) {
    expect_error(create_engine(first_player = player), "first_player")
  }
  expect_error(engine_info(NULL), "engine")
  expect_error(clear_engine(list()), "engine")
  expect_error(minimax(reset_board(), 1L, TRUE, engine = 1), "engine")
  restored <- unserialize(serialize(create_engine(memory_mb = 1), NULL))
  expect_error(engine_info(restored), "recreate")
  expect_error(minimax(matrix(0L, 5, 7), 1L, TRUE), "6x7")
  expect_error(minimax(reset_board(), -1L, TRUE), "depth")
})
