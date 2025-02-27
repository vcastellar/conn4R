% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iniciar_partida.R
\name{iniciar_partida}
\alias{iniciar_partida}
\title{Iniciar una partida de conecta 4}
\usage{
iniciar_partida(
  profundidad = 5,
  turno = 1,
  profAdaptative = TRUE,
  algCpp = FALSE
)
}
\arguments{
\item{profundidad}{profundidad de búsqueda del algoritmo minimax.}

\item{turno}{qué jugador comienza la partida: 1 para humano, 2 para IA.}
}
\description{
inicia una partida de conecta 4. Se especifica la profundidad
             de búsqueda y el jugador que inicia la partida
}
\examples{
lo siguiente inicia una partida en la que  el jugador humano es el primero en jugar
iniciar_partida(profundidad = 7, turno = 2)

para que sea la IA quien realice la primera jugada:
iniciar_partida(turno = 2)
}
