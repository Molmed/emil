#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

// Referring to the generated functions in RcppExports.cpp
extern SEXP emil_is_constant_character(SEXP, SEXP);
extern SEXP emil_is_constant_complex(SEXP, SEXP);
extern SEXP emil_is_constant_numeric(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"emil_is_constant_character", (DL_FUNC) &emil_is_constant_character, 2},
    {"emil_is_constant_complex",   (DL_FUNC) &emil_is_constant_complex,   2},
    {"emil_is_constant_numeric",   (DL_FUNC) &emil_is_constant_numeric,   2},
    {NULL, NULL, 0}
};

void R_init_emil(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

