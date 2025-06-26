surv_logloss = function(truth, distr, eps = 1e-15, IPCW = TRUE, train = NULL, ...) {
  event = truth[, 2L] == 1
  all_times = truth[, 1L]
  event_times = truth[event, 1L]

  # Bypass distr6 construction if underlying distr represented by array
  if (inherits(distr, "array")) {
    surv = distr
    if (length(dim(surv)) == 3L) {
      # survival 3d array, extract median
      surv = .ext_surv_mat(arr = surv, which.curve = 0.5)
    }
    times = as.numeric(colnames(surv))

    # calculate pdf (probability of event) at the given time of event
    # or censoring for each observation
    convert_to_pdf = getFromNamespace("cdfpdf", ns = "distr6")
    pdf = convert_to_pdf(cdf = 1 - surv)
    extend_times_pdf = getFromNamespace("C_Vec_WeightedDiscretePdf", ns = "distr6")
    pred = diag(
      extend_times_pdf(x = all_times, data = times, pdf = t(pdf))
    )
  } else {
    if (inherits(distr, c("Matdist", "Arrdist"))) {
      pred = diag(distr$pdf(truth[, 1L]))
    } else {
      pred = as.numeric(distr$pdf(data = matrix(truth[, 1L], nrow = 1L)))
    }
  }

  if (!IPCW) {
    # set any '0' predictions to a small non-zero value (to avoid log(0))
    # return -log(pdf) for all predictions
    pred[pred == 0] = eps
    return(-log(pred))
  }

  # Remove all censored observations
  pred = as.numeric(pred)[event]

  # Estimate censoring distribution using Kaplan-Meier
  if (is.null(train)) {
    km_fit = survival::survfit(Surv(truth[, "time"], 1 - truth[, "status"]) ~ 1)
  } else {
    km_fit = survival::survfit(Surv(train[, "time"], 1 - train[, "status"]) ~ 1)
  }

  # Get survival matrix from KM
  surv_km = matrix(rep(km_fit$surv, length(truth)), ncol = length(km_fit$time),
                   nrow = length(truth), byrow = TRUE)

  # Remove all censored observations
  surv_km = surv_km[event, ]

  # calculate KM survival at event times
  extend_times_cdf = getFromNamespace("C_Vec_WeightedDiscreteCdf", ns = "distr6")
  cens = diag(
    extend_times_cdf(x = event_times, data = km_fit$time, cdf = t(1 - surv_km), FALSE, FALSE)
  )

  # avoid divide by 0 errors
  cens[cens == 0] = eps

  # apply IPCW
  pred = pred / cens

  # avoid log 0 errors
  pred[pred == 0] = eps

  # return negative log-likelihood
  -log(pred)
}

# Compute per-observation prediction errors for uncensored survival times.
#
# This function returns either squared or absolute differences between predicted
# event times and observed event times, for all uncensored observations.
# Useful for downstream aggregation (e.g., RMSE, MAE), or inspection of
# prediction residuals.
obs_surv_errors = function(truth, response, method = "squared") {
  method = assert_choice(method, c("squared", "abs"))
  assert_surv(truth)

  is_event = truth[, 2L] == 1
  if (!any(is_event)) return(NA) # if no events, can't calculate score!
  event_times = truth[is_event, 1L]
  pred_times  = response[is_event]

  errors = switch(method,
    squared = (event_times - pred_times)^2,
    abs = abs(event_times - pred_times)
  )

  errors
}
