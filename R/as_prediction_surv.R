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
#' learner = mlr_learners$get("surv.coxph")
#' learner$train(task)
#' p = learner$predict(task)
#'
#' # convert to a data.table
#' tab = as.data.table(p)
#'
#' # convert back to a Prediction
#' as_prediction_surv(tab)
#'
#' # split data.table into a list of data.tables based
#' # on their survival times (ignoring censoring)
#' tabs = split(tab, cut(tab$time, 3))
#'
#' # convert back to list of predictions
#' preds = lapply(tabs, as_prediction_surv)
#'
#' # calculate performance in each group
#' sapply(preds, function(p) p$score())
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
  optional = c("crank", "lp", "distr")
  assert_names(names(x), must.include = mandatory)
  assert_names(names(x), subset.of = c(mandatory, optional))

  if ("distr" %in% names(x)) {
    distr = x$distr[[1]]
  } else {
    distr = NULL
  }

  invoke(PredictionSurv$new,
    truth = Surv(x$time, x$status),
    distr = distr,
    .args = x[, -intersect(c("time", "status", "distr"), names(x)), with = FALSE],
  )
}
