# conn4R

<!-- badges: start -->
<!-- badges: end -->

**conn4R** is an R package for playing and analyzing games of **Connect 4**
against an artificial intelligence. The search engine is written in C++
(via [Rcpp](https://www.rcpp.org/)) and implements **minimax with alpha-beta
pruning**, a transposition table, quiescence search and a static position
evaluation. The R layer provides the game interface, position entry and
analysis, and visualization with `ggplot2`.

## Installation

```r
# install.packages("remotes")
remotes::install_github("vcastellar/conn4r")
```

The package needs a C++ compiler (included in Rtools on Windows or in the
system development tools on macOS/Linux).

## Board convention

The board is an integer matrix with 6 rows and 7 columns. Each cell is
`0` (empty), `1` (human piece) or `2` (AI piece). Pieces fall by gravity to
the lowest free row of the column.

## Usage

Each of the three main tasks has a shortcut function.

### 1. Play a game

```r
library(conn4R)

# Human vs AI (the human starts, depth 6)
start_game(depth = 6)

# The AI opens the game
start_game(turn = 2)

# The AI plays against itself
start_game(auto = TRUE)
```

In games against the AI, the human chooses their move by **clicking with the
mouse on any cell of the desired column** of the plotted board. If the
graphics device does not support clicks (or with `start_game(mouse = FALSE)`),
the column is typed on the keyboard.

### 2. Enter a position

`create_position()` builds a board by replaying a sequence of moves
(columns) that alternate between the players, checking that all of them are
legal.

```r
# Central opening and alternating replies (the human opens)
board <- create_position(c(4, 4, 3, 5, 3))
visualize_board(board)
```

### 3. Analyze the position with minimax

`analyze_position()` computes the recommended move, the score, the principal
variation and a readable verdict. The player to move can be given or
inferred.

```r
analysis <- analyze_position(board, depth = 8)
analysis
#> Position analysis (C++ minimax engine)
#> ---------------------------------------------------------
#> To move:             AI (player 2)
#> Depth:               8
#> Recommended move:    column 3
#> ...

analysis$move        # recommended column
analysis$variation   # principal variation

# Replay the principal variation on the board
analyze_position(board, depth = 8, show = TRUE)
```

### Reuse analysis between moves

Each `start_game()` creates a fresh transposition table and keeps it for the
whole game. Its default budget is **64 MiB**, configurable with
`start_game(memory_mb = 128)`. The budget covers the table buffer; plots,
search stacks and other R objects use additional memory.

For repeated position analysis, create an engine and pass it to each call:

```r
engine <- create_engine(memory_mb = 64, first_player = 1)
board <- create_position(c(4, 4, 3))
analysis <- analyze_position(board, depth = 6, engine = engine)
board <- make_move(board, analysis$move, analysis$turn)
analyze_position(board, depth = 5, engine = engine)

engine_info(engine)  # capacity, occupied entries, searches and cache hits
clear_engine(engine) # empty the table without reallocating it
```

The cache distinguishes the board and player to move. Results searched to an
equal or greater depth can be reused, so a shallow request can benefit from
an earlier deeper search. When buckets fill, older and shallower entries are
preferred for replacement. Eviction can shorten the reported principal
variation. Calling `minimax()` or `analyze_position()` without an engine still
performs an independent search with a temporary table.

Create a separate engine for each game or configuration. `first_player = 2`
lets `analyze_position()` infer turns for AI-first positions; an explicit
`turn` still takes precedence. Engines live in the current R session and
must be recreated after loading a saved session. Assigning an engine to
another variable shares its cache. The evaluation formula is unchanged.

## Main functions

| Function | Description |
|---|---|
| `start_game()` | Play a full game (human vs AI or AI vs AI). |
| `create_position()` | Enter a position from a sequence of moves. |
| `analyze_position()` | Analyze a position with the minimax engine. |
| `minimax()` | Low-level search engine (C++). |
| `create_engine()` / `clear_engine()` / `engine_info()` | Create, clear and inspect a reusable search cache. |
| `evaluate_position()` | Static evaluation of a position (C++). |
| `visualize_board()` / `visualize_variation()` | Draw positions and variations. |
| `reset_board()` | Create an empty board. |
| `create_random_position()` | Generate a random legal position. |

## License

GPL-3.
