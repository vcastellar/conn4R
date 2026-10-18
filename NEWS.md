# conn4R (development version)

* The human player now enters their moves by clicking with the mouse on any
  cell of the desired column of the board (`human_turn()` and `start_game()`
  gain the argument `mouse = TRUE`). If the graphics device does not support
  clicks, or `mouse = FALSE` is used, the column is still typed on the
  keyboard.

## Translation into English (breaking change)

The whole package has been translated into English: documentation, code
comments, messages, printed output and plot labels, as well as the names of
functions, arguments, returned elements and source files. Code written for
earlier versions must be updated with the following equivalences.

### Functions and classes

| Before | Now |
|---|---|
| `analizar_posicion()` | `analyze_position()` |
| `crear_posicion()` | `create_position()` |
| `crear_posicion_aleatoria()` | `create_random_position()` |
| `iniciar_partida()` | `start_game()` |
| `reiniciar_tablero()` | `reset_board()` |
| `turno_humano()` | `human_turn()` |
| `visualizar_tablero()` | `visualize_board()` |
| `visualizar_variante()` | `visualize_variation()` |
| `evaluar_posicion()` | `evaluate_position()` |
| `juego_terminado()` | `game_over()` |
| `realizar_jugada()` | `make_move()` |
| `jugadas_disponibles()` | `available_moves()` |
| `ordenar_jugadas()` | `order_moves()` |
| class `analisis_conn4R` | class `conn4R_analysis` |

`minimax()` keeps its name.

### Arguments

| Before | Now |
|---|---|
| `tablero` | `board` |
| `turno` | `turn` |
| `turno_inicial` | `first_player` |
| `profundidad` | `depth` |
| `profAdaptative` | `adaptiveDepth` |
| `maximizandoIA` | `maximizingAI` |
| `jugada`, `jugadas` | `move`, `moves` |
| `ultima_jugada` | `last_move` |
| `variante` | `variation` |
| `mostrar` | `show` |
| `lapso` | `delay` |
| `raton` | `mouse` |
| `columna`, `jugador` | `column`, `player` |

### Elements of the returned lists and data frames

| Before | Now |
|---|---|
| `puntuacion` | `score` |
| `jugada`, `jugadas` | `move`, `moves` |
| `nodos`, `nodos_normales`, `nodos_tacticos` | `nodes`, `normal_nodes`, `tactical_nodes` |
| `variante` | `variation` |
| `evaluacion` | `evaluation` |
| `veredicto` | `verdict` |
| `turno`, `profundidad` | `turn`, `depth` |
| `finalizado`, `terminado` | `finished` |
| `resultado` | `result` |

# conn4R 0.2.0

## New functions

* `crear_posicion()` builds a board from a sequence of moves (columns),
  checking that they are legal. It is the recommended way to *enter a
  position* that you then want to analyze or continue.
* `analizar_posicion()` is a high-level interface on top of `minimax()`: it
  returns the recommended move, the score, the principal variation and a
  readable verdict, infers the turn when it is not given and, optionally,
  replays the principal variation. It comes with a `print()` method.

## CRAN preparation

* `library(ggplot2)` inside `visualizar_tablero()` is replaced by explicit
  namespace imports (`@importFrom`).
* Package-level documentation is added (`?conn4R`).
* The `DESCRIPTION` metadata now uses `Authors@R` and includes the `URL` and
  `BugReports` fields.
* The obsolete `hello` help page is removed.
* The `\value` section of `crear_posicion_aleatoria()` is fixed and now
  correctly states that it returns a matrix.

# conn4R 0.1.3

* Connect 4 engine with minimax, alpha-beta pruning, transposition table and
  quiescence search implemented in C++ (Rcpp).
