#include <R.h>
#include <Rinternals.h>

SEXP c_cindex(SEXP time_, SEXP status_, SEXP x_) {
    const double * time = REAL(time_);
    const int * status = LOGICAL(status_);
    const double * x = REAL(x_);
    const R_len_t n = length(x_);

    int pairs = 0;
    double score = 0.0;

    for (R_len_t i = 0; i < n - 1; i++) {
        if (status[i]) {
            for (R_len_t j = i + 1; j < n; j++) {
                if (time[i] < time[j]) {
                    pairs++;
                    if (x[j] > x[i]) {
                        score += 1.0;
                    } else if (x[j] == x[i]) {
                        score += 0.5;
                    }
                }
            }
        }
    }

    if (pairs == 0)
        error("Unable to calculate concordance index. No events, or all survival times are identical.");
    return ScalarReal(score / pairs);
}
