#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool c_assert_surv(const NumericMatrix& mat) {
    for (int i = 0; i < mat.nrow(); i++) {
        if (mat(i, 0) < 0 || mat(i, 0) > 1) {
            // check first element
            return false;
        }

        for (int j = 1; j < mat.ncol(); j++) {
            // S(t) in [0,1]
            if (mat(i, j) < 0 || mat(i, j) > 1) {
                return false;
            }

            // S(t) should not increase!
            if (mat(i, j) > mat(i, j - 1)) {
                return false;
            }
        }
    }

  return true;
}
