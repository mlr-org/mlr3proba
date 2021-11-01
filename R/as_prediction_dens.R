#' @title Convert to a Density Prediction
#'
#' @description
#' Convert object to a [PredictionDens].
#'
#' @inheritParams mlr3::as_prediction
#'
#' @return [PredictionDens].
#' @export
#' @examples
#' library(mlr3)
#' task = tsk("precip")
#' learner = lrn("dens.hist")
#' learner$train(task)
#' p = learner$predict(task)
#'
#' # convert to a data.table
#' tab = as.data.table(p)
#'
#' # convert back to a Prediction
#' as_prediction_dens(tab)
as_prediction_dens = function(x, ...) {
  UseMethod("as_prediction_dens")
}


#' @rdname as_prediction_dens
#' @export
as_prediction_dens.PredictionDens = function(x, ...) { # nolint
  x
}


#' @rdname as_prediction_dens
#' @export
as_prediction_dens.data.frame = function(x, ...) { # nolint
  mandatory = c("row_ids")
  optional = c("pdf", "cdf", "distr")

  assert_names(names(x), must.include = mandatory)
  assert_names(names(x), subset.of = c(mandatory, optional))

  if ("distr" %in% names(x)) {
    distr = x$distr[[1]]
  } else {
    distr = NULL
  }

  invoke(PredictionDens$new,
    distr = distr,
    .args = x[, -"distr", with = FALSE],
  )
}
