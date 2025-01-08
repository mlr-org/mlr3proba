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
#'   Small error value to prevent errors resulting from a log(0) or 1/0 calculation.
#'   Default is 1e-15 for log loss and 1e-3 for Graf.
#' @param task ([TaskSurv])
#' @param row_ids (`integer()`) \cr
#'   Passed to `Learner$predict`.
#' @param newdata (`data.frame()`) \cr
#'   If not missing `Learner$predict_newdata` is called instead of `Learner$predict`.
#' @param train_task ([TaskSurv]) \cr
#'   If not NULL then passed to measures for computing estimate of censoring distribution on
#'   training data.
#' @param train_set (`numeric()`) \cr
#'   If not NULL then passed to measures for computing estimate of censoring distribution on
#'   training data.
#' @param proper (`logical(1)`) \cr
#'   Passed to [MeasureSurvGraf] or [MeasureSurvIntLogloss].
#' @param ... Additional arguments.
#'
#' @details If `times` and `n` are missing then `measure` is evaluated over all observed time-points
#' from the [PredictionSurv] or [TaskSurv] object. If a range is provided for `times` without `n`,
#' then all time-points between the range are returned.
#'
#' @examplesIf mlr3misc::require_namespaces(c("ggplot2"), quietly = TRUE)
#' \dontrun{
#'   #' library(mlr3)
#'   task = tsk("rats")
#'
#'   # Prediction Error Curves for prediction object
#'   learn = lrn("surv.coxph")
#'   p = learn$train(task)$predict(task)
#'   pecs(p)
#'   pecs(p, measure = "logloss", times = c(20, 40, 60, 80)) +
#'     ggplot2::geom_point() +
#'     ggplot2::ggtitle("Logloss Prediction Error Curve for Cox PH")
#'
#'   # Access underlying data
#'   x = pecs(p)
#'   x$data
#'
#'   # Prediction Error Curves for fitted learners
#'   learns = lrns(c("surv.kaplan", "surv.coxph"))
#'   lapply(learns, function(x) x$train(task))
#'   pecs(learns, task = task, measure = "logloss", times = c(20, 90), n = 10)
#' }
#'
#' @export
pecs = function(x, measure = c("graf", "logloss"), times, n, eps = NULL, ...) {
  require_namespaces("ggplot2")

  if (!missing(times)) assert_numeric(times, min.len = 1L)
  if (!missing(n)) assert_integerish(n, len = 1L)

  UseMethod("pecs", x)
}

#' @rdname pecs
#' @export
pecs.list = function(x, measure = c("graf", "logloss"), times, n, eps = NULL, task = NULL, # nolint
  row_ids = NULL, newdata = NULL, train_task = NULL, train_set = NULL,
  proper = TRUE, ...) {

  measure = match.arg(measure)

  if (is.null(eps)) {
    eps = if (measure == "graf") 1e-3 else 1e-15
  } else {
    assert_numeric(eps, lower = -1, upper = 1)
  }

  assert_learners(x)
  if (any(map_lgl(x, function(y) is.null(y$model)))) {
    stopf("`x` must be a list of trained survival learners")
  }
  assert_class(task, "TaskSurv")

  if (is.null(newdata)) {
    p = lapply(x, function(y) y$predict(task = task, row_ids = row_ids))
  } else {
    p = lapply(x, function(y) y$predict_newdata(newdata = newdata, task = task))
  }

  true_times = sort(unique(task$truth()[, "time"]))

  times = .pec_times(true_times = true_times, times = times, n = n)
  if (length(times) <= 1L) {
    stopf(
      "Not enough `times` in the true observed times range: %s",
      paste0("[", toString(round(range(true_times), 3)), "]")
    )
  }

  n = as.integer(!is.null(train_task)) + as.integer(!is.null(train_set))
  if (n == 1L) {
    stop("Either 'train_task' and 'train_set' should be passed to measure or neither.")
  } else if (n) {
    train = train_task$truth(train_set)
  } else {
    train = NULL
  }

  if (measure == "logloss") {
    scores = lapply(p, function(y) {
      integrated_score(score = weighted_survival_score("intslogloss",
        truth = task$truth(),
        distribution = y$data$distr,
        times = times,
        eps = eps, train = train,
        proper = proper),
      integrated = FALSE)
    })
  } else {
    scores = lapply(p, function(y) {
      integrated_score(score = weighted_survival_score("graf",
        truth = task$truth(),
        distribution = y$data$distr,
        times = times, train = train, eps = eps,
        proper = proper),
      integrated = FALSE)
    })
  }

  times = as.numeric(names(scores[[1L]]))
  scores = round(rbindlist(list(scores)), 4)
  setnames(scores, map_chr(x, function(y) gsub("surv.", "", y$id, fixed = TRUE)))
  scores$time = times
  scores = melt(scores, "time", value.name = measure, variable.name = "learner")

  ggplot2::ggplot(data = scores, aes(x = .data[["time"]], color = .data[["learner"]],
    y = .data[[measure]])) +
    ggplot2::geom_line()
}

#' @rdname pecs
#' @export
pecs.PredictionSurv = function(x, measure = c("graf", "logloss"), times, n, eps = 1e-15, # nolint
  train_task = NULL, train_set = NULL, proper = TRUE, ...) {

  measure = match.arg(measure)
  if (is.null(eps)) {
    eps = if (measure == "graf") 1e-3 else 1e-15
  } else {
    assert_numeric(eps, lower = -1, upper = 1)
  }

  true_times = sort(unique(x$truth[, 1L]))
  times = .pec_times(true_times = true_times, times = times, n = n)

  n = as.integer(!is.null(train_task)) + as.integer(!is.null(train_set))
  if (n == 1L) {
    stop("Either 'train_task' and 'train_set' should be passed to measure or neither.")
  } else if (n) {
    train = train_task$truth(train_set)
  } else {
    train = NULL
  }

  if (measure == "logloss") {
    scores = data.frame(logloss = integrated_score(
      score = weighted_survival_score("intslogloss",
        truth = x$truth,
        distribution = x$data$distr,
        times = times,
        eps = eps, train = train, proper = proper),
      integrated = FALSE))
  } else {
    scores = data.frame(graf = integrated_score(
      score = weighted_survival_score("graf",
        truth = x$truth,
        distribution = x$data$distr,
        times = times, train = train, eps = eps,
        proper = proper),
      integrated = FALSE))
  }

  scores$time = round(as.numeric(rownames(scores)), 3)
  rownames(scores) = NULL

  ggplot2::ggplot(data = scores, aes(x = .data[["time"]], y = .data[[measure]])) +
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
    if (length(times) == 2L) {
      if (missing(n)) {
        return(true_times[true_times >= times[1L] & true_times <= times[2L]])
      } else {
        return(seq(times[1L], times[2L], length.out = n))
      }
    } else {
      return(times)
    }
  }
}
