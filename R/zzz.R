.onLoad <- function(libname, pkgname) {
  
  lineas_posibles <<- generar_coordenadas_lineas()

}

.onLoad <- function(libname, pkgname) {
  # precomputar índices lineales
  lineas_idx <<- generar_indices_posibles(lineas_posibles)
}