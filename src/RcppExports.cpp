// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// crearBitboards
NumericMatrix crearBitboards();
RcppExport SEXP _conn4R_crearBitboards() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(crearBitboards());
    return rcpp_result_gen;
END_RCPP
}
// evaluarLinea
double evaluarLinea(NumericVector linea, int turno);
RcppExport SEXP _conn4R_evaluarLinea(SEXP lineaSEXP, SEXP turnoSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type linea(lineaSEXP);
    Rcpp::traits::input_parameter< int >::type turno(turnoSEXP);
    rcpp_result_gen = Rcpp::wrap(evaluarLinea(linea, turno));
    return rcpp_result_gen;
END_RCPP
}
// evaluarTurno
double evaluarTurno(NumericMatrix tablero, int turno);
RcppExport SEXP _conn4R_evaluarTurno(SEXP tableroSEXP, SEXP turnoSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type tablero(tableroSEXP);
    Rcpp::traits::input_parameter< int >::type turno(turnoSEXP);
    rcpp_result_gen = Rcpp::wrap(evaluarTurno(tablero, turno));
    return rcpp_result_gen;
END_RCPP
}
// evaluarPosicion
double evaluarPosicion(NumericMatrix tablero);
RcppExport SEXP _conn4R_evaluarPosicion(SEXP tableroSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type tablero(tableroSEXP);
    rcpp_result_gen = Rcpp::wrap(evaluarPosicion(tablero));
    return rcpp_result_gen;
END_RCPP
}
// jugadasDisponibles
IntegerVector jugadasDisponibles(NumericMatrix tablero);
RcppExport SEXP _conn4R_jugadasDisponibles(SEXP tableroSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type tablero(tableroSEXP);
    rcpp_result_gen = Rcpp::wrap(jugadasDisponibles(tablero));
    return rcpp_result_gen;
END_RCPP
}
// juegoTerminado
List juegoTerminado(NumericMatrix tablero);
RcppExport SEXP _conn4R_juegoTerminado(SEXP tableroSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type tablero(tableroSEXP);
    rcpp_result_gen = Rcpp::wrap(juegoTerminado(tablero));
    return rcpp_result_gen;
END_RCPP
}
// realizarJugada
NumericMatrix realizarJugada(NumericMatrix tablero, int columna, int jugador);
RcppExport SEXP _conn4R_realizarJugada(SEXP tableroSEXP, SEXP columnaSEXP, SEXP jugadorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type tablero(tableroSEXP);
    Rcpp::traits::input_parameter< int >::type columna(columnaSEXP);
    Rcpp::traits::input_parameter< int >::type jugador(jugadorSEXP);
    rcpp_result_gen = Rcpp::wrap(realizarJugada(tablero, columna, jugador));
    return rcpp_result_gen;
END_RCPP
}
// miniMaxCpp
List miniMaxCpp(NumericMatrix tablero, int profundidad, bool maximizandoIA, double alpha, double beta);
RcppExport SEXP _conn4R_miniMaxCpp(SEXP tableroSEXP, SEXP profundidadSEXP, SEXP maximizandoIASEXP, SEXP alphaSEXP, SEXP betaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type tablero(tableroSEXP);
    Rcpp::traits::input_parameter< int >::type profundidad(profundidadSEXP);
    Rcpp::traits::input_parameter< bool >::type maximizandoIA(maximizandoIASEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    rcpp_result_gen = Rcpp::wrap(miniMaxCpp(tablero, profundidad, maximizandoIA, alpha, beta));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_conn4R_crearBitboards", (DL_FUNC) &_conn4R_crearBitboards, 0},
    {"_conn4R_evaluarLinea", (DL_FUNC) &_conn4R_evaluarLinea, 2},
    {"_conn4R_evaluarTurno", (DL_FUNC) &_conn4R_evaluarTurno, 2},
    {"_conn4R_evaluarPosicion", (DL_FUNC) &_conn4R_evaluarPosicion, 1},
    {"_conn4R_jugadasDisponibles", (DL_FUNC) &_conn4R_jugadasDisponibles, 1},
    {"_conn4R_juegoTerminado", (DL_FUNC) &_conn4R_juegoTerminado, 1},
    {"_conn4R_realizarJugada", (DL_FUNC) &_conn4R_realizarJugada, 3},
    {"_conn4R_miniMaxCpp", (DL_FUNC) &_conn4R_miniMaxCpp, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_conn4R(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
