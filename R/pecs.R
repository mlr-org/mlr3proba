#' @title Prediction Error Curves for PredictionSurv and LearnerSurv
#' @description Methods to plot prediction error curves (pecs) for either a [PredictionSurv] object
#' or a list of trained [LearnerSurv]s.
#' @param x ([PredictionSurv] or `list` of [LearnerSurv]s)
#' @param measure (`character(1)`) \cr
#'   Either `"graf"` for [MeasureSurvGraf], or `"logloss"` for [MeasureSurvIntLogloss]
#' @param times (`numeric()`) \cr
#'   If provided then either a vector of time-points to evaluate `measure` or a range of
#'   time-points.
#' @param n (`integer()`) \cr
#'   If `times` is missing or given as a range, then `n` provide number of time-points to evaluate
#'    `measure` over.
#' @param eps (`numeric()`) \cr
#'   Small error value to pass to [MeasureSurvIntLogloss] to prevent errors resulting from a log(0)
#'   calculation.
#' @param task ([TaskSurv])
#' @param row_ids (`integer()`) \cr
#'   Passed to `Learner$predict`.
#' @param newdata (`data.frame()`) \cr
#'   If not missing `Learner$predict_newdata` is called instead of `Learner$predict`.
#' @param ... Additional arguments.
#'
#' @details If `times` and `n` are missing then `measure` is evaluated over all observed time-points
#' from the [PredictionSurv] or [TaskSurv] object. If a range is provided for `times` without `n`,
#' then all time-points between the range are returned.
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#' #' library(mlr3)
#' task = tsk("rats")
#'
#' # Prediction Error Curves for prediction object
#' learn = lrn("surv.coxph")
#' p = learn$train(task)$predict(task)
#' pecs(p)
#' pecs(p, measure = "logloss", times = c(20, 40, 60, 80)) +
#'   ggplot2::geom_point() +
#'   ggplot2::ggtitle("Logloss Prediction Error Curve for Cox PH")
#'
#' # Access underlying data
#' x = pecs(p)
#' x$data
#'
#' # Prediction Error Curves for fitted learners
#' learns = lrns(c("surv.kaplan", "surv.coxph"))
#' lapply(learns, function(x) x$train(task))
#' pecs(learns, task = task, measure = "logloss", times = c(20, 90), n = 10)
#' }
#' }
#'
#' @export
pecs = function(x, measure = c("graf", "logloss"), times, n, eps = 1e-15, ...) {
  mlr3misc::require_namespaces("ggplot2")

  if (!missing(times)) assertNumeric(times, min.len = 1)
  if (!missing(n)) assertIntegerish(n, len = 1)
  assertNumeric(eps, lower = -1, upper = 1)

  UseMethod("pecs", x)
}

#' @rdname pecs
#' @export
pecs.list = function(x, measure = c("graf", "logloss"), times, n, eps = 1e-15, task = NULL,  # nolint
                     row_ids = NULL, newdata, ...) {

  measure = match.arg(measure)

  assert(all(sapply(x, function(y) !is.null(y$model))),
         "x must be a list of trained survival learners")
  assertClass(task, "TaskSurv")

  if (missing(newdata)) {
    p = lapply(x, function(y) y$predict(task = task, row_ids = row_ids))
  } else {
    p = lapply(x, function(y) y$predict_newdata(newdata = newdata, task = task))
  }

  true_times = sort(unique(task$truth()[, "time"]))

  times = .pec_times(true_times = true_times, times = times, n = n)
  if (length(times) <= 1) {
    stop(sprintf(
      "Not enough `times` in the true observed times range: %s",
      paste0("[", paste0(round(range(true_times), 3), collapse = ", "), "]")))
  }

  if (measure == "logloss") {
    scores = lapply(p, function(y){
      integrated_score(score = weighted_survival_score("intslogloss",
                                                       truth = task$truth(),
                                                       distribution = y$distr,
                                                       times = times,
                                                       eps = eps),
                       integrated = FALSE)
    })
  } else {
    scores = lapply(p, function(y){
      integrated_score(score = weighted_survival_score("graf",
                                                       truth = task$truth(),
                                                       distribution = y$distr,
                                                       times = times),
                       integrated = FALSE)
    })
  }

  times = as.numeric(names(scores[[1]]))
  scores = round(rbindlist(list(scores)), 4)
  colnames(scores) = sapply(x, function(y) gsub("surv.", "", y$id, fixed = TRUE))
  scores$time = times
  scores = melt(scores, "time", value.name = measure, variable.name = "learner")

  ggplot2::ggplot(data = scores, ggplot2::aes_string(x = "time", color = "learner", y = measure)) +
    ggplot2::geom_line()
}

#' @rdname pecs
#' @export
pecs.PredictionSurv = function(x, measure = c("graf", "logloss"), times, n, eps = 1e-15, ...) { # nolint

  measure = match.arg(measure)

  true_times = sort(unique(x$truth[, 1]))
  times = .pec_times(true_times = true_times, times = times, n = n)


  if (measure == "logloss") {
    scores = data.frame(logloss = integrated_score(
      score = weighted_survival_score("intslogloss",
                                      truth = x$truth,
                                      distribution = x$distr,
                                      times = times,
                                      eps = eps),
      integrated = FALSE))
  } else {
    scores = data.frame(graf = integrated_score(
      score = weighted_survival_score("graf",
                                      truth = x$truth,
                                      distribution = x$distr,
                                      times = times),
      integrated = FALSE))
  }

  scores$time = round(as.numeric(rownames(scores)), 3)
  rownames(scores) = NULL

  ggplot2::ggplot(data = scores, ggplot2::aes_string(x = "time", y = measure)) +
    ggplot2::geom_line()
}

.pec_times = function(true_times, times, n) {
  if (missing(times)) {
    if (missing(n)) {
      return(true_times)
    } else {
      return(true_times[seq(1, length(true_times), length.out = n)])
    }
  } else {
    times[times > max(true_times)] = max(true_times)
    times[times < min(true_times)] = min(true_times)
    times = sort(unique(times))
    if (length(times) == 2) {
      if (missing(n)) {
        return(true_times[true_times >= times[1] & true_times <= times[2]])
      } else {
        return(seq(times[1], times[2], length.out = n))
      }
    } else {
      return(times)
    }
  }
}
