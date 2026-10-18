# Palette and measurements shared by the visualization functions. The radii
# are expressed in cell units (each cell is 1 x 1).
.board_style <- list(
  background       = "#F4F6FA",
  board            = "#2155CD",
  board_border     = "#173E9C",
  shadow           = "#0B1B3F",
  hole             = "#E8EDF5",
  hole_border      = "#163A8F",
  human            = "#FFC93C",
  ai               = "#E84855",
  piece_border     = "#00000038",
  highlight        = "#FFFFFF",
  text             = "#0F172A",
  text_soft        = "#64748B",
  hole_radius      = 0.42,
  piece_radius     = 0.39,
  inner_radius     = 0.27,
  last_move_radius = 0.465,
  winning_radius   = 0.085
)

# Axis limits of the board plot (cell units). They are shared
# with human_turn() to translate a mouse click into a column.
.board_limits <- list(x = c(0.3, 7.7), y = c(0.25, 7.2))

#' Detect and return the coordinates of the winning line
#'
#' @description Scans all the possible lines of 4 on the board and returns
#'   the plotting coordinates of the first winning line found
#'   (score greater than or equal to 10,000), or \code{NULL} if there is none.
#'   Used internally by \code{\link{visualize_board}} to highlight the
#'   winning pieces.
#'
#' @param board 6 x 7 matrix with the state of the board.
#'
#' @return Data frame with columns \code{x} (height, 1 for the bottom row),
#'   \code{y} (column) and \code{piece} (value 3) for the 4 cells of the
#'   winning line, ordered from one end to the other, or \code{NULL} if there
#'   is no winner.
.detect_lines <- function(board) {
  directions <- rbind(c(0L, 1L), c(1L, 0L), c(1L, 1L), c(1L, -1L))

  for (row in seq_len(6L)) {
    for (column in seq_len(7L)) {
      player <- board[row, column]
      if (player == 0L) next

      for (i in seq_len(nrow(directions))) {
        rows <- row + (0:3) * directions[i, 1]
        columns <- column + (0:3) * directions[i, 2]
        if (any(rows < 1L | rows > 6L | columns < 1L | columns > 7L)) next

        if (all(board[cbind(rows, columns)] == player)) {
          return(data.frame(x = 7L - rows, y = columns, piece = 3L))
        }
      }
    }
  }

  NULL
}

# Vertices of a circle for each center (cx, cy). They are drawn as polygons
# so that their size depends on the axes and not on the graphics device. The
# columns passed in `...` are replicated across all the vertices of their circle.
.circles <- function(cx, cy, r, ..., n = 64L) {
  angle <- seq(0, 2 * pi, length.out = n + 1L)[-1L]
  id <- rep(seq_along(cx), each = n)
  df <- data.frame(
    x     = cx[id] + r * cos(angle),
    y     = cy[id] + r * sin(angle),
    group = id,
    cx    = cx[id],
    cy    = cy[id]
  )
  for (name in names(list(...))) {
    v <- list(...)[[name]]
    df[[name]] <- if (length(v) == 1L) rep(v, nrow(df)) else v[id]
  }
  df
}

# Outline of a rectangle with corners rounded with radius `r`.
.rounded_rectangle <- function(xmin, xmax, ymin, ymax, r, n = 16L) {
  corner <- function(cx, cy, from) {
    angle <- seq(from, from + pi / 2, length.out = n)
    data.frame(x = cx + r * cos(angle), y = cy + r * sin(angle))
  }
  rbind(
    corner(xmax - r, ymax - r, 0),
    corner(xmin + r, ymax - r, pi / 2),
    corner(xmin + r, ymin + r, pi),
    corner(xmax - r, ymin + r, 3 * pi / 2)
  )
}

#' Visualize the Connect 4 board
#'
#' @description Generates a \pkg{ggplot2} plot of the current state of the
#'   board in a flat, modern style: a blue panel with rounded corners and a
#'   shadow, light holes for the empty cells, amber pieces for
#'   player 1 (human) and coral pieces for player 2 (AI). The pieces are drawn
#'   in data coordinates, so the board adapts to the size of the
#'   graphics device.
#'
#'   The subtitle summarizes the state of the game (number of moves, win or
#'   draw). If there is a winning line, a white line is drawn over its
#'   four pieces and the other pieces are dimmed. When \code{last_move} is
#'   given, that piece is surrounded by a white ring and its column
#'   number is highlighted.
#'
#' @param board 6 x 7 matrix with the state of the board. Cells: 0 empty,
#'   1 human, 2 AI.
#' @param last_move Coordinates \code{c(row, column)} of the last piece
#'   placed, or \code{NULL} so as not to highlight it.
#'
#' @return A \code{ggplot} object ready to be printed with \code{print()} or
#'   \code{plot()}.
#'
#' @examples
#' board <- create_random_position(21)
#' p <- visualize_board(board)
#' print(p)
#'
#' # Empty board
#' print(visualize_board(reset_board()))
#'
#' @seealso \code{\link{create_random_position}}, \code{\link{reset_board}}
#'
#' @export
visualize_board <- function(board, last_move = NULL) {
  if (!is.null(last_move) &&
      (length(last_move) != 2L || anyNA(last_move) ||
       any(!is.finite(last_move)) ||
       any(last_move != as.integer(last_move)) ||
       !last_move[[1]] %in% 1:6 || !last_move[[2]] %in% 1:7 ||
       board[last_move[[1]], last_move[[2]]] == 0L)) {
    stop("`last_move` must point to a piece on the board as c(row, column).",
         call. = FALSE)
  }

  style <- .board_style

  # Cells in plotting coordinates: x = column, y = height (1 at the bottom).
  cells <- data.frame(
    cx    = rep(1:7, each = 6),
    cy    = rep(6:1, times = 7),
    piece = as.vector(board)
  )
  occupied <- cells[cells$piece != 0L, ]

  line <- .detect_lines(board)
  winning <- rep(FALSE, nrow(occupied))
  if (!is.null(line)) {
    winning <- paste(occupied$cx, occupied$cy) %in% paste(line$y, line$x)
  }
  # With a winning line, the rest of the pieces are dimmed to highlight it.
  opacity <- if (is.null(line)) rep(1, nrow(occupied)) else ifelse(winning, 1, 0.4)

  df_frame <- .rounded_rectangle(0.38, 7.62, 0.38, 6.62, r = 0.32)
  df_shadow <- transform(df_frame, x = x + 0.05, y = y - 0.10)
  df_holes <- .circles(cells$cx, cells$cy, style$hole_radius)
  df_pieces <- .circles(
    occupied$cx, occupied$cy, style$piece_radius,
    player = factor(occupied$piece, levels = 1:2, labels = c("Human", "AI")),
    alpha  = opacity
  )
  df_inner <- .circles(occupied$cx, occupied$cy, style$inner_radius,
                       alpha = opacity)

  df_columns <- data.frame(x = 1:7, y = 6.98, label = as.character(1:7))

  # State of the game for the subtitle.
  n_moves <- sum(board != 0L)
  if (!is.null(line)) {
    winner <- board[7L - line$x[[1]], line$y[[1]]]
    status <- if (winner == 1L) "Human wins" else "AI wins"
  } else if (n_moves == 42L) {
    status <- "Draw: board full"
  } else {
    status <- sprintf("%d %s", n_moves, if (n_moves == 1L) "move" else "moves")
  }
  if (!is.null(last_move)) {
    status <- sprintf("%s  \u00b7  Last move: column %d",
                      status, as.integer(last_move[[2]]))
  }

  p <- ggplot() +
    geom_polygon(data = df_shadow, aes(x = x, y = y),
                 fill = style$shadow, alpha = 0.18) +
    geom_polygon(data = df_frame, aes(x = x, y = y),
                 fill = style$board, colour = style$board_border, linewidth = 0.6) +
    geom_polygon(data = df_holes, aes(x = x, y = y, group = group),
                 fill = style$hole, colour = style$hole_border, linewidth = 0.4)

  # The pieces layer is added even when there are none so that the legend
  # always appears and the board does not change size during the game.
  p <- p +
    geom_polygon(data = df_pieces,
                 aes(x = x, y = y, group = group, fill = player, alpha = alpha),
                 colour = style$piece_border, linewidth = 0.5, key_glyph = "point") +
    geom_polygon(data = df_inner,
                 aes(x = x, y = y, group = group, alpha = alpha * 0.08),
                 fill = "black", colour = NA)

  if (!is.null(last_move)) {
    df_last <- .circles(
      as.integer(last_move[[2]]), 7L - as.integer(last_move[[1]]),
      style$last_move_radius, element = "last_move"
    )
    p <- p + geom_polygon(data = df_last, aes(x = x, y = y, group = group),
                          fill = NA, colour = style$highlight, linewidth = 1.3)
  }

  if (!is.null(line)) {
    df_line <- data.frame(x = line$y[[1]], y = line$x[[1]],
                          xend = line$y[[4]], yend = line$x[[4]])
    df_dots <- .circles(line$y, line$x, style$winning_radius,
                        element = "winning_line")
    p <- p +
      geom_segment(data = df_line, aes(x = x, y = y, xend = xend, yend = yend),
                   colour = style$highlight, alpha = 0.9, linewidth = 2.2,
                   lineend = "round") +
      geom_polygon(data = df_dots, aes(x = x, y = y, group = group),
                   fill = style$highlight, colour = NA)
  }

  p <- p +
    geom_text(data = df_columns, aes(x = x, y = y, label = label),
              colour = style$text_soft, size = 3.6)

  if (!is.null(last_move)) {
    p <- p + geom_text(data = df_columns[last_move[[2]], ],
                       aes(x = x, y = y, label = label),
                       colour = style$board, size = 4.2, fontface = "bold")
  }

  p +
    scale_fill_manual(
      values = c(Human = style$human, AI = style$ai),
      limits = c("Human", "AI"),
      drop   = FALSE,
      name   = NULL
    ) +
    scale_alpha_identity() +
    guides(fill = guide_legend(
      override.aes = list(shape = 21, size = 6, stroke = 0.6,
                        colour = style$piece_border, alpha = 1)
    )) +
    coord_fixed(xlim = .board_limits$x, ylim = .board_limits$y,
                expand = FALSE) +
    labs(title = "Connect 4", subtitle = status) +
    theme_void(base_size = 12) +
    theme(
      plot.background   = element_rect(fill = style$background, colour = NA),
      panel.background  = element_rect(fill = style$background, colour = NA),
      plot.title        = element_text(hjust = 0.5, face = "bold", size = 18,
                                       colour = style$text,
                                       margin = margin(b = 4)),
      plot.subtitle     = element_text(hjust = 0.5, size = 11,
                                       colour = style$text_soft,
                                       margin = margin(b = 6)),
      legend.position   = "bottom",
      legend.text       = element_text(colour = style$text, size = 11,
                                       margin = margin(r = 14, l = 2)),
      legend.key        = element_rect(fill = NA, colour = NA),
      legend.margin     = margin(t = 2),
      plot.margin       = margin(14, 14, 10, 14)
    )
}

#' Visualize a variation played step by step
#'
#' @description Replays a sequence of moves on a board, alternating the
#'   players. It first shows the initial position using
#'   \code{\link{visualize_board}} and then, after each pause, shows the
#'   position resulting from the next move.
#'
#' @param board 6 x 7 matrix with the initial position. Cells: 0 empty,
#'   1 human, 2 AI.
#' @param turn Player who makes the first move of \code{variation}:
#'   \code{1} (human) or \code{2} (AI).
#' @param variation Vector of columns (1-7) that will be replayed in order.
#' @param delay Seconds to wait between positions. Defaults to 1 second.
#'   \code{0} can be used to replay the variation without waiting.
#'
#' @return Invisibly, the resulting 6 x 7 matrix after replaying the whole
#'   variation.
#'
#' @examples
#' \dontrun{
#' board <- reset_board()
#' visualize_variation(board, turn = 1, variation = c(4, 4, 3, 5))
#' }
#'
#' @seealso \code{\link{visualize_board}}, \code{\link{make_move}}
#'
#' @export
visualize_variation <- function(board, turn, variation, delay = 1) {
  if (!is.matrix(board) || !identical(dim(board), c(6L, 7L)) ||
      anyNA(board) || any(!board %in% 0:2)) {
    stop("`board` must be a 6x7 matrix with values 0, 1 or 2.",
         call. = FALSE)
  }
  if (length(turn) != 1L || is.na(turn) || !turn %in% c(1, 2)) {
    stop("`turn` must be 1 or 2.", call. = FALSE)
  }
  if (!is.numeric(variation) || anyNA(variation) ||
      any(!is.finite(variation)) || any(variation != as.integer(variation)) ||
      any(!variation %in% 1:7)) {
    stop("`variation` must contain integer columns between 1 and 7.",
         call. = FALSE)
  }
  if (length(delay) != 1L || !is.numeric(delay) || is.na(delay) ||
      !is.finite(delay) || delay < 0) {
    stop("`delay` must be a finite number greater than or equal to 0.",
         call. = FALSE)
  }

  current_board <- matrix(as.integer(board), nrow = 6L, ncol = 7L)
  player <- as.integer(turn)
  print(visualize_board(current_board))

  for (i in seq_along(variation)) {
    column <- as.integer(variation[[i]])
    if (!column %in% available_moves(current_board)) {
      stop(sprintf("Move %d is not valid: column %d is full.",
                   i, column), call. = FALSE)
    }

    Sys.sleep(delay)
    current_board <- make_move(current_board, column, player)
    row <- min(which(current_board[, column] != 0L))
    print(visualize_board(current_board, c(row, column)))
    player <- 3L - player
  }

  invisible(current_board)
}
