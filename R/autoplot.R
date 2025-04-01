#' @title Plot for Survival Tasks
#'
#' @description
#' Generates plots for [mlr3proba::TaskSurv], depending on argument `type`:
#'   * `"target"`: Calls [GGally::ggsurv()] on a [survival::survfit()] object.
#'   This computes the **Kaplan-Meier survival curve** for the observations if this task.
#'   * `"duo"`: Passes data and additional arguments down to [GGally::ggduo()].
#'     `columnsX` is target, `columnsY` is features.
#'   * `"pairs"`: Passes data and additional arguments down to
#'   [GGally::ggpairs()].
#'     Color is set to target column.
#'
#' @param object ([mlr3proba::TaskSurv]).
#' @param type (`character(1)`):\cr
#'   Type of the plot. See above for available choices.
#' @template param_theme
#' @param reverse (`logical()`)\cr
#' If `TRUE` and `type = 'target'`, it plots the Kaplan-Meier curve of the
#' censoring distribution. Default is `FALSE`.
#' @param ... (`any`):
#'   Additional arguments.
#'   `rhs` is passed down to `$formula` of [mlr3proba::TaskSurv] for stratification
#'   for type `"target"`. Other arguments are passed to the respective underlying plot
#'   functions.
#'
#' @return [ggplot2::ggplot()] object.
#'
#' @template section_theme
#'
#' @examplesIf mlr3misc::require_namespaces(c("mlr3viz", "ggplot2"), quietly = TRUE)
#' library(mlr3)
#' library(mlr3viz)
#' library(mlr3proba)
#' library(ggplot2)
#'
#' task = tsk("lung")
#' task$head()
#'
#' autoplot(task) # KM
#' autoplot(task) # KM of the censoring distribution
#' autoplot(task, rhs = "sex")
#' autoplot(task, type = "duo")
#' @export
autoplot.TaskSurv = function(object, type = "target", theme = theme_minimal(), reverse = FALSE, ...) { # nolint
  assert_choice(type, choices = c("target", "duo", "pairs"), null.ok = FALSE)
  require_namespaces(c("survival", "GGally"))

  switch(type,
    "target" = {
      ddd = list(...)

      sf = survival::survfit(
        object$formula(rhs = ddd$rhs %??% 1, reverse = reverse),
        data = object$data()
      )

      plot = GGally::ggsurv(sf, remove_named(ddd, "rhs"))
      plot + theme
    },

    "duo" = {
      GGally::ggduo(object$data(),
        columnsX = object$target_names,
        columnsY = object$feature_names, ...) +
        theme
    },

    "pairs" = {
      GGally::ggpairs(object$data(), ...) +
        theme
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
plot.TaskSurv = function(x, ...) {
  print(autoplot(x, ...))
}

#' @title Plot for Density Tasks
#'
#' @description
#' Generates plots for [mlr3proba::TaskDens].
#'
#' @param object ([mlr3proba::TaskDens]).
#' @param type (`character(1)`):
#'   Type of the plot. Available choices:
#'   * `"dens"`: histogram density estimator (default) with [ggplot2::geom_histogram()].
#'   * `"freq"`: histogram frequency plot with [ggplot2::geom_histogram()].
#'   * `"overlay"`: histogram with overlaid density plot with [ggplot2::geom_histogram()] and
#'   [ggplot2::geom_density()].
#'   * `"freqpoly"`: frequency polygon plot with [ggplot2::geom_freqpoly].
#' @template param_theme
#' @param ... (`any`):
#'   Additional arguments, possibly passed down to the underlying plot functions.
#' @return [ggplot2::ggplot()] object.
#'
#' @template section_theme
#'
#' @examplesIf mlr3misc::require_namespaces(c("mlr3viz", "ggplot2"), quietly = TRUE)
#' library(mlr3)
#' library(mlr3proba)
#' library(mlr3viz)
#' library(ggplot2)
#' task = tsk("precip")
#' task$head()
#'
#' autoplot(task, bins = 15)
#' autoplot(task, type = "freq", bins = 15)
#' autoplot(task, type = "overlay", bins = 15)
#' autoplot(task, type = "freqpoly", bins = 15)
#' @export
autoplot.TaskDens = function(object, type = "dens", theme = theme_minimal(), ...) { # nolint
  assert_choice(type, c("dens", "freq", "overlay", "freqpoly"), null.ok = FALSE)

  p = ggplot(data = object$data(), aes(x = .data[[object$feature_names]]), ...)

  switch(type,
    "dens" = {
      p +
        geom_histogram(aes(y = after_stat(density)), fill = "white", color = "black", ...) +
        ylab("Density") +
        theme
    },

    "freq" = {
      p + geom_histogram(fill = "white", color = "black", ...) +
        ylab("Count") +
        theme
    },

    "overlay" = {
      p +
        geom_histogram(aes(y = after_stat(density)), colour = "black", fill = "white", ...) +
        geom_density(alpha = 0.2, fill = "#5dadc8") +
        ylab("Density") +
        theme
    },

    "freqpoly" = {
      p +
        geom_freqpoly(...) +
        theme
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
plot.TaskDens = function(x, ...) {
  print(autoplot(x, ...))
}

#' @title Plot for PredictionSurv
#'
#' @description
#' Generates plots for [mlr3proba::PredictionSurv], depending on argument `type`:
#'
#' - `"calib"` (default): **Calibration plot** comparing the average predicted
#' survival distribution (`Pred`) to a Kaplan-Meier prediction (`KM`), this is
#' *not* a comparison of a stratified `crank` or `lp`.
#' - `"dcalib"`: **Distribution calibration plot**.
#' A model is considered D-calibrated if, for any given quantile `p`, the
#' proportion of observed outcomes occurring before the predicted time quantile,
#' matches `p`. For example, 50% of events should occur before the predicted
#' median survival time (i.e. the time corresponding to a predicted survival
#' probability of 0.5).
#' Good calibration means that the resulting line plot will lie close to the
#' straight line \eqn{y = x}.
#' Note that we impute `NA`s from the predicted quantile function with the
#' maximum observed outcome time.
#' - `"scalib"`: **Smoothed calibration plot** at a specific time point.
#' For a range of probabilities of event occurrence in \eqn{[0,1]} (x-axis),
#' the y-axis has the smoothed observed proportions calculated using hazard
#' regression (model is fitted using the predicted probabilities).
#' See Austin et al. (2020) and [MeasureSurvICI] for more details.
#' Good calibration means that the resulting line plot will lie close to the
#' straight line \eqn{y = x}.
#' - `"isd"`: Plot the predicted **i**ndividual **s**urvival **d**istributions
#' (survival curves) for the test set's observations.
#'
#' @section Notes:
#'
#' 1. `object` must have a `distr` prediction, as all plot `type`s use the
#' predicted survival distribution/matrix.
#' 2. `type = "dcalib"` is drawn a bit differently from Haider et al. (2020),
#' though its still conceptually the same.
#'
#' @param object ([mlr3proba::PredictionSurv]).
#' @param type (`character(1)`) \cr
#'  Type of the plot, see Description.
#' @param row_ids (`integer()`) \cr
#'  If `type = "isd"`, specific observation ids (from the test set) for which
#'  we draw their predicted survival distributions.
#' @param times (`numeric()`) \cr
#'  If `type = "calib"` then `times` is the values on the x-axis to plot over.
#'  If `NULL`, we use all time points from the predicted survival matrix (`object$data$distr`).
#' @param cuts (`integer(1)`) \cr
#'  If `type = "calib"`, number of cuts in \eqn{(0,1)}, which define the bins on
#'  the x-axis of the D-calibration plot. Default is `11`.
#' @param time (`numeric(1)`) \cr
#'  If `type = "scalib"`, a specific time point at which the smoothed calibration
#'  plot is constructed.
#' @template param_theme
#' @param ... (`any`):
#'   Additional arguments, currently unused.
#'
#' @template section_theme
#'
#' @references
#' `r format_bib("haider_2020", "austin_2020")`
#'
#' @examplesIf mlr3misc::require_namespaces(c("mlr3viz", "ggplot2"), quietly = TRUE)
#' library(mlr3)
#' library(mlr3proba)
#' library(mlr3viz)
#'
#' learner = lrn("surv.coxph")
#' task = tsk("gbcs")
#' p = learner$train(task, row_ids = 1:600)$predict(task, row_ids = 601:686)
#'
#' # calibration by comparison of average prediction to Kaplan-Meier
#' autoplot(p)
#'
#' # same as above, use specific time points
#' autoplot(p, times = seq(1, 1000, 5))
#'
#' # Distribution-calibration (D-Calibration)
#' autoplot(p, type = "dcalib")
#'
#' # Smoothed Calibration (S-Calibration)
#' autoplot(p, type = "scalib", time = 1750)
#'
#' # Predicted survival curves (all observations)
#' autoplot(p, type = "isd")
#'
#' # Predicted survival curves (specific observations)
#' autoplot(p, type = "isd", row_ids = c(601, 651, 686))
#'
#' @export
autoplot.PredictionSurv = function(object, type = "calib",
  times = NULL, row_ids = NULL, cuts = 11L, time = NULL, theme = theme_minimal(), ...) {
  assert_choice(type, c("calib", "dcalib", "scalib", "isd"), null.ok = FALSE)
  assert("distr" %in% object$predict_types)
  assert_number(cuts, na.ok = FALSE, lower = 1L, null.ok = FALSE)
  assert_numeric(row_ids, any.missing = FALSE, lower = 1, null.ok = TRUE)

  switch(type,
    "calib" = {
      # get predicted survival matrix
      if (inherits(object$data$distr, "array")) {
        surv = object$data$distr
        if (length(dim(surv)) == 3L) {
          # survival 3d array, extract median
          surv = .ext_surv_mat(arr = surv, which.curve = 0.5)
        }
      } else {
        stop("Distribution prediction does not have a survival matrix or array
             in the $data$distr slot")
      }
      # get predicted time points
      pred_times = as.numeric(colnames(surv))
      # which time points to use for plotting
      times = times %??% pred_times

      # function to request S(t) for points "in-between"
      extend_times = getFromNamespace("C_Vec_WeightedDiscreteCdf", ns = "distr6")
      # rows => times, cols => obs
      surv2 = extend_times(times, pred_times, cdf = t(1 - surv), FALSE, FALSE)

      # average predicted probability across test set observations
      pred_surv = rowMeans(surv2)

      # fit a Kaplan-Meier on the test data
      km_fit = survival::survfit(object$truth ~ 1)
      # make a S(t) one-column matrix (by default same probability for every observation)
      km_surv = matrix(km_fit$surv, ncol = 1) # rows => times
      # get KM's S(t) at the predicted time points
      km_surv = extend_times(times, km_fit$time, cdf = 1 - km_surv, FALSE, FALSE)[,1]

      data = data.table(
        x = times,
        y = c(km_surv, pred_surv),
        Group = rep(c("KM", "Pred"), each = length(times))
      )

      ggplot(data, aes(x = .data[["x"]], y = .data[["y"]], group = .data[["Group"]],
                       color = .data[["Group"]])) +
        geom_line() +
        labs(x = "Time", y = "Average Survival Probability") +
        theme +
        theme(legend.title = element_blank())
    },

    "dcalib" = {
      p = seq.int(0, 1, length.out = cuts)
      true_times = object$truth[, 1L]
      q = map_dbl(p, function(.x) {
        # time points at which observations had `.x` survival
        qi = as.numeric(object$distr$quantile(.x))
        qi[is.na(qi)] = max(true_times)
        sum(true_times <= qi) / length(object$row_ids)
      })

      ggplot(data = data.table(p, q), aes(x = p, y = q)) +
        geom_bar(stat = "identity", fill = "#5dadc8") +
        geom_line(color = "black") +
        scale_x_continuous(breaks = p) +
        annotate("segment", x = 0, y = 0, xend = 1, yend = 1, alpha = 0.5,
                 linetype = "dashed") +
        labs(x = "Survival Probability (Bins)",
             y = "Observed Proportion") +
        theme
    },

    "scalib" = {
      requireNamespace("polspline")
      # test set survival outcome
      times  = object$truth[, 1L]
      status = object$truth[, 2L]
      # time point for plotting calibration curve
      time = assert_number(time, na.ok = FALSE, lower = 0, null.ok = FALSE)

      # get predicted survival matrix
      if (inherits(object$data$distr, "array")) {
        surv = object$data$distr
        if (length(dim(surv)) == 3L) {
          # survival 3d array, extract median
          surv = .ext_surv_mat(arr = surv, which.curve = 0.5)
        }
      } else {
        stop("Distribution prediction does not have a survival matrix or array
             in the $data$distr slot")
      }

      # get cdf at the specified time point
      extend_times_cdf = getFromNamespace("C_Vec_WeightedDiscreteCdf", ns = "distr6")
      pred_times = as.numeric(colnames(surv))
      cdf = as.vector(extend_times_cdf(time, pred_times, cdf = t(1 - surv), TRUE, FALSE))
      # to avoid log(0) later, same as in paper's Appendix
      cdf[cdf == 1] = 0.9999

      # get the cdf complement (survival) log-log transformed
      cll = log(-log(1 - cdf))

      hare_fit = polspline::hare(data = times, delta = status, cov = as.matrix(cll))

      # make a wide-range of cdf probabilities
      cdf_grid = seq(0.001, 0.999, 0.001)
      cll_grid = log(-log(1 - cdf_grid))

      smoothed_cdf_grid = polspline::phare(q = time, cov = cll_grid, fit = hare_fit)
      if (anyNA(smoothed_cdf_grid)) {
        warning("`polspline::phare` fit resulted in NaN smoothed probabilities")
      }

      pred = obs = NULL
      data = data.table(pred = cdf_grid, obs = smoothed_cdf_grid)

      ggplot(data, aes(x = pred, y = obs)) +
        geom_line() +
        annotate("segment", x = 0, y = 0, xend = 1, yend = 1, alpha = 0.5,
                 linetype = "dashed") +
        labs(x = "Predicted probability",
             y = "Observed probability",
             title = paste0("t = ", time)) +
        theme
    },

    "isd" = {
      surv = object$data$distr # assume this is 2d survival matrix
      data = data.table(
        row_id = as.factor(object$row_ids),
        time = rep(as.numeric(colnames(surv)), each = nrow(surv)),
        surv_prob = invoke(c, .args = as.data.table(surv))
      )

      # filter data to specific ids
      if (!is.null(row_ids)) {
        data = data[get("row_id") %in% row_ids]
      }

      p = ggplot(data, aes(x = .data[["time"]], y = .data[["surv_prob"]],
                           group = .data[["row_id"]], color = .data[["row_id"]])) +
        geom_line() +
        labs(x = "Time", y = "Survival Probability") +
        theme

      # usually too many observations, so don't draw legend
      if (is.null(row_ids)) {
        p = p + theme(legend.position = "none")
      }

      p
    },

    stopf("Unknown plot type '%s'", type)
  )
}
