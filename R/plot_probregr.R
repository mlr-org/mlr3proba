#' @title Visualise probabilistic regression distribution predictions
#' @description Plots probability density functions from `n` predicted probability
#' distributions.
#'
#' @param p ([PredictionRegr]) \cr With at least column `distr`.
#' @param n (`integer(1)`) \cr Number of predictions to plot.
#' @param type (`character(1)`) \cr One of `"point"` (default), `"line"`, `"both"`, `"none"`.
#' @param which_plot (`character(1)`) \cr One of `"random"` (default) or `"top"`. See details.
#' @param rm_zero (`logical(1)`) \cr If `TRUE` (default) does not plot points where `f(x) = 0`.
#' @param ... Unused
#'
#' @details
#' `type`:
#'
#' * `"point"` (default) - Truth plotted as point (truth, predicted_pdf(truth))
#' * `"line"` - Truth plotted as vertical line intercepting x-axis at the truth.
#' * `"both"` - Plots both the above options.
#' * `"none"` - Truth not plotted (default if `p$truth` is missing).
#'
#' `which_plot`:
#'
#' * "random"` (default) - Random selection of `n` distributions are plotted.
#' * "top"` - Top `n` distributions are plotted.
#'
#' It is unlikely the plot will be interpretable when `n >> 5`.
#'
#'
#' @examples
#' \dontrun{
#' library(mlr3verse)
#' task = tsk("boston_housing")
#' pipe = as_learner(ppl("probregr", lrn("regr.ranger"), dist = "Normal"))
#' p = pipe$train(task)$predict(task)
#' plot_probregr(p, 10, "point", "top")
#' }
#' @export
plot_probregr = function(p, n, type = c("point", "line", "both", "none"),
  which_plot = c("random", "top"), rm_zero = TRUE, ...) {

  which_plot = match.arg(which_plot)
  type = match.arg(type)

  if (allMissing(p$truth)) type = "none"

  assert(n <= length(p$row_ids))

  if (which_plot == "top") {
    which = seq(n)
  } else if (which_plot == "random") {
    which = sort(sample(length(p$row_ids), n))
  }

  d = p$distr[which]
  truth = p$truth[which]

  xmin = floor(min(truth) - 3 * max(d$stdev()))
  xmax = ceiling(max(truth) + 3 * max(d$stdev()))
  x = seq(xmin, xmax, length.out = 100)

  data_pred = suppressWarnings(cbind(x, data.table::melt(d$pdf(x))))
  if (rm_zero) data_pred[round(data_pred$value, 6) == 0, ] = NA
  variable = factor(d$strprint(), levels = d$strprint())
  data_truth = data.frame(x = truth, variable = variable)
  data_points = data.frame(x = truth, y = diag(as.matrix(d$pdf(truth))), variable = variable)

  out = ggplot(data_pred, aes(x = x, y = value, color = variable)) +
    geom_line(lwd = 1.2, na.rm = TRUE) +
    theme_minimal() +
    theme(legend.position = "n") +
    labs(x = "x", y = "f(x)")

  if (type %in% c("line", "both")) {
    out = out +
      geom_vline(aes(xintercept = x, color = variable), data_truth, lwd = 1, lty = 3)
  }

  if (type %in% c("point", "both")) {
    out = out +
      geom_point(aes(x = x, y = y, fill = variable),
              color = "black", data_points, size = 3, pch = 23)
  }

  out
}
