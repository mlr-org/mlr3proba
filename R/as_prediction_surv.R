#' @title Convert to a Survival Prediction
#'
#' @description
#' Convert object to a [PredictionSurv].
#'
#' @inheritParams mlr3::as_prediction
#'
#' @return [PredictionSurv].
#' @export
#' @examples
#' library(mlr3)
#' task = tsk("rats")
#' learner = lrn("surv.coxph")
#' learner$train(task)
#' p = learner$predict(task)
#'
#' # convert to a data.table
#' tab = as.data.table(p)
#'
#' # convert back to a Prediction
#' as_prediction_surv(tab)
as_prediction_surv = function(x, ...) {
  UseMethod("as_prediction_surv")
}


#' @rdname as_prediction_surv
#' @export
as_prediction_surv.PredictionSurv = function(x, ...) { # nolint
  x
}


#' @rdname as_prediction_surv
#' @export
as_prediction_surv.data.frame = function(x, ...) { # nolint
  mandatory = c("row_ids", "time", "status")
  optional = c("crank", "lp", "distr", "response")
  assert_names(names(x), must.include = mandatory)
  assert_names(names(x), subset.of = c(mandatory, optional))

  if ("distr" %in% names(x)) {
    distr = x$distr[[1]][[1]]
  } else {
    distr = NULL
  }

  if ("crank" %nin% names(x)) {
    if ("lp" %in% names(x)) {
      x$crank = x$lp
    } else if ("response" %in% names(x)) {
      x$crank = -x$response
    } else {
      x$crank = -apply(1 - distr, 1, function(.x) sum(c(.x[1], diff(.x)) * x$time))
    }
  }

  invoke(PredictionSurv$new,
    truth = Surv(x$time, x$status),
    distr = distr,
    .args = x[, -intersect(c("time", "status", "distr"), names(x)), with = FALSE],
  )
}
