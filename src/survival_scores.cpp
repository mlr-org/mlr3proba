#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericMatrix c_score_intslogloss(const NumericVector& truth,
                                  const NumericVector& unique_times,
                                  const NumericMatrix& cdf,
                                  double eps) {
  const int nr_obs = truth.length();
  const int nc_times = unique_times.length();
  NumericMatrix ll(nr_obs, nc_times);

  for (int i = 0; i < nr_obs; i++) {
    for (int j = 0; j < nc_times; j++) {
      const double tmp = (truth[i] > unique_times[j]) ? 1 - cdf(j, i) : cdf(j, i);
      ll(i, j) = -log(max(tmp, eps));
    }
  }

  return ll;
}

// [[Rcpp::export]]
NumericMatrix c_score_graf_schmid(const NumericVector& truth,
                                  const NumericVector& unique_times,
                                  const NumericMatrix& cdf,
                                  int power = 2) {
  const int nr_obs = truth.length();
  const int nc_times = unique_times.length();
  NumericMatrix igs(nr_obs, nc_times);

  for (int i = 0; i < nr_obs; i++) {
    for (int j = 0; j < nc_times; j++) {
      const double tmp = (truth[i] > unique_times[j]) ? cdf(j, i) : 1 - cdf(j, i);
      igs(i, j) = std::pow(tmp, power);
    }
  }

  return igs;
}

// [[Rcpp::export]]
NumericMatrix c_weight_survival_score(const NumericMatrix& score,
                                      const NumericMatrix& truth,
                                      const NumericVector& unique_times,
                                      const NumericMatrix& cens,
                                      bool proper, double eps) {
  NumericVector times = truth(_, 0);
  NumericVector status = truth(_, 1);

  NumericVector cens_times = cens(_, 0);
  NumericVector cens_surv = cens(_, 1);

  const int nr = score.nrow();
  const int nc = score.ncol();

  NumericMatrix mat(nr, nc);

  for (int i = 0; i < nr; i++) {
    double k = 0.0;
    // if censored and proper then zero-out and remove
    if (proper && status[i] == 0) {
      mat(i, _) = NumericVector(nc);
      continue;
    }

    for (int j = 0; j < nc; j++) {
      // if alive and not proper then IPC weights are current time
      if (!proper && times[i] > unique_times[j]) {
        for (int l = 0; l < cens_times.length(); l++) {
          if (unique_times[j] >= cens_times[l] &&
            (l == cens_times.length() - 1 || unique_times[j] < cens_times[l + 1])) {
            mat(i, j) = score(i, j) / cens_surv[l];
            break;
          }
        }
        // if dead (or alive and proper) weight by event time
        // if censored remove
      } else {
        if (status[i] == 0) {
          mat(i, j) = 0;
          continue;
        }

        if (k == 0) {
          for (int l = 0; l < cens_times.length(); l++) {
            // weight 1 if death occurs before first censoring time
            if ((times[i] < cens_times[l]) && l == 0) {
              k = 1;
              break;
            } else if (times[i] >= cens_times[l] &&
              (l == cens_times.length() - 1 || times[i] < cens_times[l + 1])) {
              k = cens_surv[l];
              // k == 0 only if last obs censored, therefore mat is set to 0 anyway
              // This division by eps can cause inflation of the score,
              // due to a very large value for a particular (i-obs, j-time)
              // Use 't_max' to filter 'cens' in that case
              if (k == 0) {
                k = eps;
              }
              break;
            }
          }
        }

        // weight by IPCW
        mat(i, j) = score(i, j) / k;
      }
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
