.onLoad <- function(libname, pkgname) {
  lineas_posibles <<- generar_coordenadas_lineas()
  lineas_idx      <<- generar_indices_posibles(lineas_posibles)
}
