#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern SEXP c_cindex(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"c_cindex", (DL_FUNC) &c_cindex, 3},
    {NULL, NULL, 0}
};

void R_init_mlr3proba(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
