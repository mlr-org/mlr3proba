#' @title Convert to a Competing Risk Prediction
#'
#' @description
#' Convert object to a [PredictionCompRisks].
#'
#' @inheritParams mlr3::as_prediction
#'
#' @return [PredictionCompRisks].
#' @export
#' @examples
#' library(mlr3)
#' task = tsk("pbc")
#' learner = lrn("cmprsk.aalen")
#' learner$train(task)
#' p = learner$predict(task)
#'
#' # convert to a data.table
#' tab = as.data.table(p)
#'
#' # convert back to a Prediction
#' as_prediction_cmprsk(tab)
as_prediction_cmprsk = function(x, ...) {
  UseMethod("as_prediction_cmprsk")
}

#' @rdname as_prediction_cmprsk
#' @export
as_prediction_cmprsk.PredictionCompRisks = function(x, ...) { # nolint
  x
}

#' @rdname as_prediction_cmprsk
#' @export
as_prediction_cmprsk.data.frame = function(x, ...) {
  mandatory = c("row_ids", "time", "event")
  optional = c("CIF")
  assert_names(names(x), must.include = mandatory, subset.of = c(mandatory, optional))

  cmp_event_ids = unique(unlist(lapply(x$CIF, names)))
  cif = if ("CIF" %in% names(x)) {
    # Reconstruct the list of matrices (one per competing risk)
    lapply(cmp_event_ids, function(event_id) {
      do.call(rbind, lapply(x$CIF, function(obs_cif) obs_cif[[event_id]]))
    }) |> set_names(cmp_event_ids)
  } else NULL

  invoke(
    PredictionCompRisks$new,
    truth = Surv(x$time, factor(x$event, levels = c("0", sort(cmp_event_ids)))),
    cif = cif,
    .args = x[, -intersect(c("time", "event", "CIF"), names(x)), with = FALSE]
  )
}
