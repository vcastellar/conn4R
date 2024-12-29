tablero <- crear_posicion_aleatoria(12)
visualizar_tablero(tablero)
prof = 8
system.time({
  mejor_jugada_IA <- minimax(tablero, prof, maximizandoIA = TRUE)
  print(mejor_jugada_IA)
})



system.time({
  mejor_jugada_IA <- miniMaxCpp(tablero, prof, maximizandoIA = TRUE)
  print(mejor_jugada_IA)
})

system.time({
  mejor_jugada_IA <- minimaxOpt_cpp(tablero, prof, maximizandoIA = TRUE)
  print(mejor_jugada_IA)
})