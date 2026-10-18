#' Ask the human for a move.
#'
#' @description The function prompts the human player to enter a move.
#' If the move is legal, the human's move is shown on the board as
#' a 1 in the corresponding cell. The AI's moves are shown on the
#' board as a 2.
#' @param board 6 x 7 matrix representing the state of the game board.
#' @param move an integer from 1 to 7, one for each of the 7 columns.
#' If move = NULL (the default), the player is prompted to enter their move.
#' @param mouse Logical. If \code{TRUE} (the default) and the active graphics
#'   device allows it, the move is chosen by clicking with the mouse on
#'   any cell of the desired column of the board drawn by
#'   \code{\link{visualize_board}}. If \code{FALSE}, or if the device does not
#'   support clicks, the column is typed on the keyboard.
#' @details
#' If the move is legal, the human's move is shown on the board as a 1
#' in the cell that corresponds to the move.
#' If the move entered is illegal, a legal move is requested again.
#'
#' When playing with the mouse, clicking outside the board or on a full column
#' asks for the move again. If the selection is canceled (Esc key or right
#' mouse button, depending on the device), the move is requested from the
#' keyboard.
#'
#'
#' @examples
#' board <- reset_board()
#' board <- human_turn(board, move = 4)
#' visualize_board(board)
#' # the following can also be done (not run):
#' # board <- reset_board()
#' # print(visualize_board(board))
#' # board <- human_turn(board)  # click on a column of the board
#' # visualize_board(board)



#' @export
human_turn <- function(board, move = NULL, mouse = TRUE) {

  possible_moves <- available_moves(board)

  if (length(possible_moves) == 0) {
    return(board) # No moves available
  }

  use_mouse <- is.null(move) && isTRUE(mouse) && .supports_clicks()

  repeat {

    if (!is.null(move)) {
      human_move <- move
    } else if (use_mouse) {
      cat("Your turn. Click on a column of the board.\n")
      flush.console()
      human_move <- .read_mouse_move()
      if (is.null(human_move)) {
        # Selection canceled or the click could not be read: use the keyboard.
        use_mouse <- FALSE
        next
      }
    } else {
      cat("Your turn. Choose a column (1-7): ")
      input <- readLines(con = stdin(), n = 1)
      if (length(input) == 0L) {
        stop("Could not read the move: standard input is closed.",
             call. = FALSE)
      }
      human_move <- suppressWarnings(as.integer(input))
      # human_move <- as.integer(readline())
    }

    if (length(human_move) == 1L && human_move %in% possible_moves) {
      return(make_move(board, human_move, 1L))
    } else {
      cat("Invalid move. Try again.\n")
    }
  }
}

# Tells whether the active graphics device can return the position of a click.
.supports_clicks <- function() {
  if (!interactive() || grDevices::dev.cur() == 1L) {
    return(FALSE)
  }
  capability <- tryCatch(
    grDevices::dev.capabilities("locator")$locator,
    error = function(e) NA
  )
  # Some devices (for example RStudio's) do not report this capability
  # even though they have it; it is only ruled out if they explicitly deny it.
  !isFALSE(capability)
}

# Waits for a click on the plotted board and returns the clicked column
# (NA if the click falls outside the board) or NULL if the selection is canceled.
.read_mouse_move <- function() {
  panel <- grep("^panel\\.", .viewport_names(grid::current.vpTree()),
                value = TRUE)
  if (length(panel) == 0L) {
    return(NULL) # There is no board drawn on the device.
  }

  grid::seekViewport(panel[[1]])
  on.exit(grid::upViewport(0), add = TRUE)

  click <- tryCatch(grid::grid.locator(unit = "npc"), error = function(e) NULL)
  if (is.null(click)) {
    return(NULL)
  }

  .column_from_npc(as.numeric(click$x), as.numeric(click$y))
}

# Names of all the viewports in a grid tree (the ggplot2 plot draws
# its panel in a viewport called "panel.<row>-<col>-...").
.viewport_names <- function(tree) {
  if (inherits(tree, "vpTree")) {
    c(tree$parent$name, .viewport_names(tree$children))
  } else if (inherits(tree, c("vpList", "vpStack"))) {
    unlist(lapply(tree, .viewport_names), use.names = FALSE)
  } else {
    tree$name
  }
}

# Converts a position in coordinates relative to the plot panel (0-1)
# into the board column underneath it, or NA if it falls outside.
.column_from_npc <- function(x, y) {
  lim <- .board_limits
  x_data <- lim$x[[1]] + x * diff(lim$x)
  y_data <- lim$y[[1]] + y * diff(lim$y)

  if (x_data < 0.5 || x_data >= 7.5 || y_data < 0.5 || y_data > lim$y[[2]]) {
    return(NA_integer_)
  }
  as.integer(floor(x_data + 0.5))
}
