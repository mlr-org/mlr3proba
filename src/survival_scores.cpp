#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericMatrix c_score_logloss(const NumericVector& obs_times,
                              const NumericVector& times,
                              const NumericMatrix& cdf, // [times x obs]
                              double eps) {
  const int n_obs = obs_times.length();
  const int n_times = times.length();
  NumericMatrix logloss_mat(n_obs, n_times);

  for (int i = 0; i < n_obs; i++) {
    for (int j = 0; j < n_times; j++) {
      // Use S(t) for y > t, 1 - S(t) for y <= t
      const double tmp = (obs_times[i] > times[j]) ? 1 - cdf(j, i) : cdf(j, i);
      logloss_mat(i, j) = -std::log(std::max(tmp, eps));
    }
  }

  return logloss_mat;
}

// [[Rcpp::export]]
NumericMatrix c_score_graf_schmid(const NumericVector& obs_times,
                                  const NumericVector& times,
                                  const NumericMatrix& cdf, // [times x obs]
                                  int power = 2) {
  const int n_obs = obs_times.length();
  const int n_times = times.length();
  NumericMatrix score_mat(n_obs, n_times);

  for (int i = 0; i < n_obs; i++) {
    for (int j = 0; j < n_times; j++) {
      // Use 1 - S(t) for y > t, S(t) for y <= t
      const double tmp = (obs_times[i] > times[j]) ? cdf(j, i) : 1 - cdf(j, i);
      score_mat(i, j) = std::pow(tmp, power);
    }
  }

  return score_mat;
}

// [[Rcpp::export]]
NumericMatrix c_apply_ipcw_weights(const NumericMatrix& score,
                                   const NumericMatrix& truth,
                                   const NumericVector& unique_times,
                                   const NumericMatrix& cens,
                                   double eps) {
  NumericVector times = truth(_, 0); // observed times
  NumericVector status = truth(_, 1); // event indicators (1 = event, 0 = censored)

  NumericVector cens_times = cens(_, 0); // increasing time points for G(t)
  NumericVector cens_surv = cens(_, 1); // G(t) values

  const int n_obs = score.nrow(); // number of observations (rows)
  const int n_times = score.ncol(); // number of time points (columns)

  // Check: number of time points
  if (unique_times.size() != n_times) {
    stop("Length of 'unique_times' must match number of columns in 'score'.");
  }

  // Check: number of observations
  if (truth.nrow() != n_obs) {
    stop("Number of rows in 'truth' must match number of rows in 'score'.");
  }

  NumericMatrix mat(n_obs, n_times);  // output matrix, initialized to all 0

  for (int i = 0; i < n_obs; i++) {
    const double ti = times[i];
    const int    di = status[i];

    for (int j = 0; j < n_times; j++) {
      const double tau = unique_times[j];

      // Censored and t_i <= tau => ignored (leave as 0)
      if (di == 0 && ti <= tau) continue;

      // Choose whether to weight by G(tau) or G(ti)
      double w_time = (ti > tau) ? tau : ti;
      double weight = 0.0; // IPCW

      if (w_time < cens_times[0]) {
      // G(t) = 1 (left constant interpolation)
        weight = 1.0;
      } else {
      // Find weight using left-continuous, constant interpolation of G(t)
      for (int k = 0; k < cens_times.length(); k++) {
        // Exact or in interval [cens_times[k], cens_times[k + 1]) or right interpolation
        if (w_time >= cens_times[k] &&
           (k == cens_times.length() - 1 || w_time < cens_times[k + 1])) {
            weight = cens_surv[k];
            break;
          }
        }
      }

      // Avoid divide-by-zero or score inflation for low G(t) values
      if (weight == 0.0) weight = eps;

      // Apply IPC weight
      mat(i, j) = score(i, j) / weight;
    }
  }

  return mat;
}

// [[Rcpp::export]]
float c_concordance(const NumericVector& time,
                    const NumericVector& status,
                    const NumericVector& crank,
                    double t_max,
                    const std::string& weight_meth,
                    const NumericMatrix& cens,
                    const NumericMatrix& surv,
                    float tiex) {
  double num = 0.0;
  double den = 0.0;
  double weight = -1.0;

  NumericVector cens_times;
  NumericVector cens_surv;
  int cl = 0;

  NumericVector surv_times;
  NumericVector surv_surv;
  int sl = 0;

  if (weight_meth == "G2" || weight_meth == "G" || weight_meth == "SG") {
    cens_times = cens(_, 0);
    cens_surv = cens(_, 1);
    cl = cens_times.length();
  }
  if (weight_meth == "S" || weight_meth == "SG") {
    surv_times = surv(_, 0);
    surv_surv = surv(_, 1);
    sl = surv_times.length();
  }

  for (int i = 0; i < time.length() - 1; i++) {
    weight = -1;
    if (status[i] == 1) {
      for (int j = i + 1; j < time.length(); j++) {
        if (time[i] < time[j] && time[i] < t_max) {
          if (weight == -1) {
            if (weight_meth == "I") {
              weight = 1;
            } else if (weight_meth == "G2" || weight_meth == "G" ||
              weight_meth == "SG") {
              for (int l = 0; l < cl; l++) {
                if (time[i] >= cens_times[l] && ((l == cl - 1) || time[i] < cens_times[l + 1])) {
                  if (weight_meth == "G" || weight_meth == "SG") {
                    weight = pow(cens_surv[l], -1);
                  } else {
                    weight = pow(cens_surv[l], -2);
                  }
                  break;
                }
              }
            }

            if (weight_meth == "SG" || weight_meth == "S") {
              for (int l = 0; l < sl; l++) {
                if (time[i] >= surv_times[l] &&
                  (l == sl - 1 || time[i] < surv_times[l + 1])) {
                  if (weight_meth == "S") {
                    weight = surv_surv[l];
                  } else {
                    weight *= surv_surv[l];
                  }
                  break;
                }
              }
            }
          }

          den += weight;

          if (crank[i] > crank[j]) {
            num += weight;
          } else if (crank[i] == crank[j]) {
            num += tiex * weight;
          }
        }
      }
    }
  }

  if (den == 0) {
    Rcpp::stop("Unable to calculate concordance index. No events, or all survival times are identical.");
  }

  return num / den;
}

// [[Rcpp::export]]
double c_gonen(const NumericVector& crank, float tiex) {
  // NOTE: we assume crank to be sorted!
  const int n = crank.length();
  double ghci = 0.0;

  for (int i = 0; i < n - 1; i++) {
    const double ci = crank[i];
    for (int j = i + 1; j < n; j++) {
      const double cj = crank[j];
      ghci += ((ci < cj) ? 1 : tiex) / (1 + exp(ci - cj));
    }
  }

  return (2 * ghci) / (n * (n - 1));
}
