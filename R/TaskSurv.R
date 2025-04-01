#' @title Survival Task
#'
#' @description
#' This task specializes [mlr3::Task] and [mlr3::TaskSupervised] for
#' **single-event survival** problems.
#' The target is comprised of survival times and an event indicator (\eqn{0}
#' represents censored observations, \eqn{1} represents observations that had the
#' event).
#' Every row corresponds to one subject/observation.
#'
#' Predefined tasks are stored in [mlr3::mlr_tasks].
#'
#' The `task_type` is set to `"surv"`.
#'
#' **Note:** Currently only right-censoring is supported, though it possible to
#' create tasks with left and interval censoring using the [Surv][survival::Surv()]
#' interface.
#'
#' @template param_rows
#'
#' @references
#' `r format_bib("grambsch_1994")`
#'
#' @family Task
#' @examples
#' library(mlr3)
#' task = tsk("lung")
#'
#' # meta data
#' task$target_names # target is always (time, status) for right-censoring tasks
#' task$feature_names
#' task$formula()
#'
#' # survival data
#' task$truth() # survival::Surv() object
#' task$times() # (unsorted) times
#' task$status() # event indicators (1 = death, 0 = censored)
#' task$unique_times() # sorted unique times
#' task$unique_event_times() # sorted unique event times
#' task$kaplan(strata = "sex") # stratified Kaplan-Meier
#' task$kaplan(reverse = TRUE) # Kaplan-Meier of the censoring distribution
#'
#' # proportion of censored observations across all dataset
#' task$cens_prop()
#' # proportion of censored observations at or after the 95% time quantile
#' task$admin_cens_prop(quantile_prob = 0.95)
#' # proportion of variables that are significantly associated with the
#' # censoring status via a logistic regression model
#' task$dep_cens_prop() # 0 indicates independent censoring
#' # data barely satisfies proportional hazards assumption (p > 0.05)
#' task$prop_haz()
#' # veteran data is definitely non-PH (p << 0.05)
#' tsk("veteran")$prop_haz()
#' @export
TaskSurv = R6Class("TaskSurv",
  inherit = TaskSupervised,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @details
    #' Depending on the censoring type (`"type"`), the output of a survival
    #' task's `"$target_names"` is a `character()` vector with values the names
    #' of the target columns.
    #' Specifically, the output is as follows (and in the specified order):
    #'
    #' - For `type` = `"right"` or `"left"`: (`"time"`, `"event"`)
    #' - For `type` = `"interval"`: (`"time"`, `"time2"`)
    #'
    #' @template param_id
    #' @template param_backend
    #' @template param_time
    #' @template param_event
    #' @template param_time2
    #' @template param_type
    #' @param label (`character(1)`)\cr
    #'  Label for the new instance.
    initialize = function(id, backend, time = "time", event = "event", time2 = "time2",
                          type = "right", label = NA_character_) {
      cens_type = assert_choice(type, c("right", "left", "interval"))
      private$.cens_type = cens_type
      backend = as_data_backend(backend)

      if (cens_type == "interval") {
        super$initialize(
          id = id, task_type = "surv", backend = backend,
          target = c(time, time2), label = label
        )
      } else {
        # check event is 0 or 1
        event_col = get_private(backend)$.data[, event, with = FALSE][[1L]]
        assert_integerish(event_col, lower = 0L, upper = 1L, any.missing = FALSE)

        super$initialize(
          id = id, task_type = "surv", backend = backend,
          target = c(time, event), label = label
        )
      }
    },

    #' @description
    #' True response for specified `row_ids`. This is the survival outcome
    #' using the [Surv][survival::Surv()] format and depends on the censoring
    #' type. Defaults to all rows with role `"use"`.
    #'
    #' For censoring type:
    #'
    #' - `"right|left"`: `Surv(time, event, type = "right|left")`
    #' - `"interval"`: `Surv(time, time2, type = "interval2")`
    #'
    #' @return [survival::Surv()].
    truth = function(rows = NULL) {
      tn = self$target_names
      cens_type = self$cens_type
      data = self$data(rows, cols = self$target_names)
      args = list(time = data[[tn[1L]]])

      if (cens_type == "interval") {
        args$time2 = data[[tn[2L]]]
        # the "interval" type in `Surv()` has the the event
        # and is not so much used, "interval2" is the one we want here
        args$type = "interval2"
      } else {
        args$event = as.integer(data[[tn[2L]]])
        args$type = cens_type # "right" or "left"
      }

      if (allMissing(args$event) & allMissing(args$time)) {
        # this is to pass autotest...
        suppressWarnings(invoke(Surv, .args = args))
      } else {
        invoke(Surv, .args = args)
      }
    },

    #' @description
    #' Creates a formula for survival models with [survival::Surv()] on the LHS
    #' (left hand side).
    #'
    #' @param rhs
    #' If `NULL`, RHS (right hand side) is `"."`, otherwise RHS is `"rhs"`.
    #' @param reverse
    #' If `TRUE` then formula calculated with 1 - status. Only applicable to `"right"`
    #' or `"left"` censoring.
    #'
    #' @return [stats::formula()].
    formula = function(rhs = NULL, reverse = FALSE) {
      # formula appends the rhs argument to Surv(time, (time2), event)~
      tn = self$target_names
      cens_type = self$cens_type

      if (cens_type == "interval") {
        lhs = sprintf("Surv(%s, %s, type = '%s')", tn[1L], tn[2L], "interval2")
      } else {
        if (reverse) {
          lhs = sprintf("Surv(%s, 1 - %s, type = '%s')", tn[1L], tn[2L], cens_type)
        } else {
          lhs = sprintf("Surv(%s, %s, type = '%s')", tn[1L], tn[2L], cens_type)
        }
      }

      formulate(lhs, rhs %??% ".", env = getNamespace("survival"))
    },

    #' @description
    #' Returns the (unsorted) outcome times.
    #' @return `numeric()`
    times = function(rows = NULL) {
      if (self$cens_type == "interval") stop("Not supported for interval-censored data")

      truth = self$truth(rows)

      as.numeric(truth[, 1L])
    },

    #' @description
    #' Returns the event indicator (aka censoring/survival indicator).
    #' If censoring type is `"right"` or `"left"` then `1` is event and `0` is censored.
    #' If censoring type is `"interval"` then `0` means right-censored, `1` is
    #' event, `2` is left-censored and `3` is interval-censored.
    #'
    #' See [survival::Surv()].
    #'
    #' @return `integer()`
    status = function(rows = NULL) {
      truth = self$truth(rows)

      if (self$cens_type == "interval") {
        status = truth[, 3L]
      } else {
        status = truth[, 2L]
      }

      as.integer(status)
    },

    #' @description
    #' Returns the sorted unique outcome times.
    #' @return `numeric()`
    unique_times = function(rows = NULL) {
      sort(unique(self$times(rows)))
    },

    #' @description
    #' Returns the sorted unique event (or failure) outcome times.
    #' @return `numeric()`
    unique_event_times = function(rows = NULL) {
      sort(unique(self$times(rows)[self$status(rows) == 1])) # event => 1
    },

    #' @description
    #' Calls [survival::survfit()] to calculate the Kaplan-Meier estimator.
    #'
    #' @param strata (`character()`)\cr
    #'   Stratification variables to use.
    #' @param rows (`integer()`)\cr
    #'   Subset of row indices.
    #' @param reverse (`logical()`)\cr
    #' If `TRUE` calculates Kaplan-Meier of censoring distribution (1-status). Default `FALSE`.
    #' @param ... (any)\cr
    #'   Additional arguments passed down to [survival::survfit.formula()].
    #' @return [survival::survfit.object].
    kaplan = function(strata = NULL, rows = NULL, reverse = FALSE, ...) {
      assert_character(strata, null.ok = TRUE)
      f = self$formula(strata %??% 1, reverse)
      cols = c(self$target_names, intersect(self$backend$colnames, strata))
      data = self$data(rows = rows, cols = cols)
      survival::survfit(f, data = data, ...)
    },

    #' @description
    #' Returns the same task with the status variable reversed, i.e., 1 - status.
    #'
    #' @return [mlr3proba::TaskSurv].
    reverse = function() {
      if (self$cens_type == "interval") stop("Not supported for interval-censored data")

      data = copy(self$data())
      data[, (self$target_names[2L]) := 1 - get(self$target_names[2L])]
      as_task_surv(data, time = self$target_names[1L], event = self$target_names[2L],
                   type = self$cens_type, id = paste0(self$id, "_reverse"))
    },

    #' @description
    #' Returns the **proportion of censoring** for this survival task.
    #' This the proportion of censored observations in case of `"right"` or
    #' `"left"` censoring, otherwise the proportion of left (2), right (0) and
    #' interval censored (3) observations when censoring type is `"interval"`.
    #'
    #' By default, this is returned for all observations, otherwise only the
    #' specified ones (`rows`).
    #'
    #' @return `numeric()`
    cens_prop = function(rows = NULL) {
      status = self$status(rows)

      tbl_status = table(status, exclude = 1) # 1 => event
      cens_props = as.vector(tbl_status) / length(status)

      if (self$cens_type == "interval") {
        set_names(cens_props, names(tbl_status)) # can be 0, 2 or 3, see `$status()`
      } else {
        cens_props
      }
    },

    #' @description
    #' Returns an estimated proportion of **administratively censored
    #' observations** (i.e. censored at or after a user-specified time point).
    #' Our main assumption here is that in an administratively censored dataset,
    #' the maximum censoring time is likely close to the maximum event time and
    #' so we expect higher proportion of censored subjects near the study end date.
    #'
    #' Only designed for `"right"` censoring.
    #'
    #' @param admin_time (`numeric(1)`) \cr
    #' Administrative censoring time (in case it is known *a priori*).
    #' @param quantile_prob (`numeric(1)`) \cr
    #' Quantile probability value with which we calculate the cutoff time for
    #' administrative censoring. Ignored, if `admin_time` is given.
    #' By default, `quantile_prob` is equal to \eqn{0.99}, which translates to a
    #' time point very close to the maximum outcome time in the dataset.
    #' A lower value will result in an earlier time point and therefore in a more
    #' *relaxed* definition (i.e. higher proportion) of administrative censoring.
    #'
    #' @return `numeric()`
    admin_cens_prop = function(rows = NULL, admin_time = NULL, quantile_prob = 0.99) {
      if (self$cens_type != "right") stopf("Not supported for %s-censored data", self$cens_type)

      assert_number(quantile_prob, lower = 0.8, upper = 1, null.ok = FALSE)
      assert_number(admin_time, lower = 0, null.ok = TRUE)

      times = self$times(rows)
      status = self$status(rows)

      # Get administrative time
      if (is.null(admin_time)) {
        t_max = unname(round(stats::quantile(times, probs = quantile_prob)))
      } else {
        t_max = min(admin_time, max(times))
      }

      # Identify total censored observations
      total_censored = sum(status == 0)
      if (total_censored == 0) return(0)

      # Count the number of observations censored at or after the max time
      admin_censored = sum(status == 0 & times >= t_max)

      # proportion of administrative censoring
      admin_censored / total_censored
    },

    #' @description
    #' Returns the proportion of covariates (task features) that are found to be
    #' significantly associated with censoring.
    #' This function fits a logistic regression model via [glm][stats::glm] with
    #' the censoring status as the response and using all features as predictors.
    #' If a covariate is significantly associated with the censoring status,
    #' it suggests that censoring may be *informative* (dependent) rather than
    #' *random* (non-informative).
    #' This methodology is more suitable for **low-dimensional datasets** where
    #' the number of features is relatively small compared to the number of
    #' observations.
    #'
    #' Only designed for `"right"` censoring.
    #'
    #' @param sign_level (`numeric(1)`) \cr
    #' Significance level for each coefficient's p-value from the logistic
    #' regression model. Default is \eqn{0.05}.
    #' @param method (`character(1)`) \cr
    #' Method to adjust p-values for multiple comparisons, see [p.adjust.methods].
    #' Default is `"holm"`.
    #'
    #' @return `numeric()`
    dep_cens_prop = function(rows = NULL, method = "holm", sign_level = 0.05) {
      if (self$cens_type != "right") stopf("Not supported for %s-censored data", self$cens_type)

      status_var = self$target_names[[2L]]
      glm_summary = summary(stats::glm(
        formula = formulate(lhs = status_var, rhs = "."),
        data = self$data(cols = c(self$feature_names, status_var)),
        family = stats::binomial(link = "logit")
      ))

      # extract the p-values
      p_values = glm_summary$coefficients[, "Pr(>|z|)"]
      p_values_adj = stats::p.adjust(p_values, method = method)
      n_coefs = length(p_values_adj) - 1 # exclude the intercept, include dummy-encoded variables
      n_signif = sum(p_values_adj[-1L] <= sign_level)

      n_signif / n_coefs
    },

    #' @description
    #' Checks if the data satisfy the *proportional hazards (PH)* assumption using
    #' the Grambsch-Therneau test, `r cite_bib("grambsch_1994")`.
    #' Uses [cox.zph][survival::cox.zph()].
    #' This method should be used only for **low-dimensional datasets** where
    #' the number of features is relatively small compared to the number of
    #' observations.
    #'
    #' Only designed for `"right"` censoring.
    #'
    #' @return `numeric()` \cr
    #' If no errors, the p-value of the global chi-square test.
    #' A p-value \eqn{< 0.05} is an indication of possible PH violation.
    prop_haz = function() {
      if (self$cens_type != "right") stopf("Not supported for %s-censored data", self$cens_type)

      cox = lrn("surv.coxph")
      cox$encapsulate("evaluate", fallback = lrn("surv.kaplan"))
      cox$train(self)
      ok = (length(cox$errors) == 0L) & (length(cox$warnings) == 0L)

      # cox model didn't converge, train didn't succeed, etc
      if (!ok) stop("Error/warning during cox model fitting")

      zph_test = survival::cox.zph(fit = cox$model)
      p_value = zph_test$table["GLOBAL", "p"]

      p_value
    }
  ),

  active = list(
    #' @field cens_type (`character(1)`)\cr
    #' Returns the type of censoring, one of `"right"`, `"left"` or `"interval"`.
    #'
    #' Currently, only the `"right"` censoring type is fully supported, the rest
    #' are experimental and the API might change in the future.
    cens_type = function(rhs) {
      assert_ro_binding(rhs)
      private$.cens_type
    }
  ),

  private = list(
    .cens_type = NULL
  )
)
