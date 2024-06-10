#' @title Survival Task
#'
#' @description
#' This task specializes [mlr3::Task] and [mlr3::TaskSupervised] for
#' possibly-censored survival problems.
#' The target is comprised of survival times and an event indicator.
#' Predefined tasks are stored in [mlr3::mlr_tasks].
#'
#' The `task_type` is set to `"surv"`.
#'
#' @template param_rows
#' @template param_id
#' @template param_backend
#'
#' @references
#' `r format_bib("grambsch_1994")`
#'
#' @family Task
#' @export
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
#' task$risk_set(time = 700) # observation ids that are not censored or dead at t = 700
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
TaskSurv = R6::R6Class("TaskSurv",
  inherit = TaskSupervised,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @details
    #' Depending on the censoring type (`"type"`), the output of a survival
    #' task's `"$target_names"` is a `character()` vector with values the names
    #' of the columns given by the above initialization arguments.
    #' Specifically, the output is as follows (and in the specified order):
    #'
    #' - For `type` = `"right"`, `"left"` or `"mstate"`: (`"time"`, `"event"`)
    #' - For `type` = `"interval"` or `"counting"`: (`"time"`, `"time2"`, `"event"`)
    #' - For `type` = `"interval2"`: (`"time"`, `"time2`)
    #'
    #' @template param_time
    #' @template param_event
    #' @template param_time2
    #' @template param_type
    #' @param label (`character(1)`)\cr
    #'    Label for the new instance.
    initialize = function(id, backend, time = "time", event = "event", time2,
      type = c("right", "left", "interval", "counting", "interval2", "mstate"),
      label = NA_character_) {

      type = match.arg(type)

      backend = as_data_backend(backend)

      if (type != "interval2") {
        c_ev = r6_private(backend)$.data[, event, with = FALSE][[1]]
        if (type == "mstate") {
          assert_factor(c_ev)
        } else if (type == "interval") {
          assert_integerish(c_ev, lower = 0, upper = 3)
        } else if (!is.logical(c_ev)) {
          assert_integerish(c_ev, lower = 0, upper = 2)
        }
      }

      private$.censtype = type

      if (type %in% c("right", "left", "mstate")) {
        super$initialize(
          id = id, task_type = "surv", backend = backend,
          target = c(time, event), label = label
        )
      } else if (type %in% c("interval", "counting")) {
        super$initialize(
          id = id, task_type = "surv", backend = backend,
          target = c(time, time2, event), label = label
        )
      } else {
        super$initialize(
          id = id, task_type = "surv", backend = backend,
          target = c(time, time2), label = label
        )
      }
    },

    #' @description
    #' True response for specified `row_ids`. This is the survival outcome
    #' using the [Surv][survival::Surv()] format and depends on the censoring
    #' type. Defaults to all rows with role `"use"`.
    #'
    #' @return [survival::Surv()].
    truth = function(rows = NULL) {
      tn = self$target_names
      ct = self$censtype
      d = self$data(rows, cols = self$target_names)
      args = list(time = d[[tn[1L]]], type = self$censtype)

      if (ct %in% c("right", "left", "mstate")) {
        args$event = as.integer(d[[tn[2L]]])
      } else if (ct %in% c("interval", "counting")) {
        args$event = as.integer(d[[tn[3L]]])
        args$time2 = d[[tn[2L]]]
      } else {
        args$time2 = d[[tn[2L]]]
      }

      if (allMissing(args$event) & allMissing(args$time)) {
        return(suppressWarnings(invoke(Surv, .args = args)))
      } else {
        return(invoke(Surv, .args = args))
      }
    },

    #' @description
    #' Creates a formula for survival models with [survival::Surv()] on the LHS
    #' (left hand side).
    #'
    #' @param rhs
    #' If `NULL`, RHS (right hand side) is `"."`, otherwise RHS is `"rhs"`.
    #' @param reverse
    #' If `TRUE` then formula calculated with 1 - status.
    #'
    #' @return [stats::formula()].
    formula = function(rhs = NULL, reverse = FALSE) {
      # formula appends the rhs argument to Surv(time, event)~
      tn = self$target_names
      if (length(tn) == 2) {
        if (reverse) {
          lhs = sprintf("Surv(%s, 1 - %s, type = '%s')", tn[1L], tn[2L], self$censtype)
        } else {
          lhs = sprintf("Surv(%s, %s, type = '%s')", tn[1L], tn[2L], self$censtype)
        }
      } else {
        lhs = sprintf("Surv(%s, %s, %s, type = '%s')", tn[1L], tn[2L], tn[3L], self$censtype)
      }
      formulate(lhs, rhs %??% ".", env = getNamespace("survival"))
    },

    #' @description
    #' Returns the (unsorted) outcome times.
    #' @return `numeric()`
    times = function(rows = NULL) {
      truth = self$truth(rows)
      if (self$censtype %in% c("interval", "counting", "interval2")) {
        return(truth[, 1:2])
      } else {
        return(as.numeric(truth[, 1L]))
      }
    },

    #' @description
    #' Returns the event indicator (aka censoring/survival indicator).
    #' If `censtype` is `"right"` or `"left"` then `1` is event and `0` is censored.
    #' If `censtype` is `"mstate"` then `0` is censored and all other values are different events.
    #' If `censtype` is `"interval"` then `0` is right-censored, `1` is event, `2` is left-censored,
    #' `3` is interval-censored.
    #' See [survival::Surv()].
    #'
    #' @return `integer()`
    status = function(rows = NULL) {
      truth = self$truth(rows)
      if (self$censtype %in% c("interval", "counting", "interval2")) {
        status = truth[, 3L]
      } else {
        status = truth[, 2L]
      }

      as.integer(status)
    },

    #' @description
    #' Returns the sorted unique outcome times for `"right"`, `"left"` and
    #' `"mstate"` types of censoring.
    #'
    #' @return `numeric()`
    unique_times = function(rows = NULL) {
      assert_choice(self$censtype, choices = c("right", "left", "mstate"))

      sort(unique(self$times(rows)))
    },

    #' @description
    #' Returns the sorted unique event (or failure) outcome times for `"right"`,
    #' `"left"` and `"mstate"` types of censoring.
    #'
    #' @return `numeric()`
    unique_event_times = function(rows = NULL) {
      assert_choice(self$censtype, choices = c("right", "left", "mstate"))

      sort(unique(self$times(rows)[self$status(rows) != 0]))
    },

    #' @description
    #' Returns the `row_ids` of the observations **at risk** (not dead or censored
    #' or had other events in case of multi-state tasks) at the specified `time`.
    #'
    #' Only designed for `"right"`, `"left"` and `"mstate"` types of censoring.
    #'
    #' @param time (`numeric(1)`) \cr Time to return risk set for, if `NULL`
    #' returns all `row_ids`.
    #'
    #' @return `integer()`
    risk_set = function(time = NULL) {
      assert_choice(self$censtype, choices = c("right", "left", "mstate"))

      if (is.null(time)) {
        self$row_ids
      } else {
        self$row_ids[self$times() >= time]
      }
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
      data = self$data(cols = cols, rows = rows)
      survival::survfit(f, data = data, ...)
    },

    #' @description
    #' Returns the same task with the status variable reversed, i.e., 1 - status.
    #' Only designed for `"left"` and `"right"` censoring.
    #'
    #' @return [mlr3proba::TaskSurv].
    reverse = function() {
      assert_choice(self$censtype, choices = c("right", "left"))

      d = copy(self$data())
      d[, (self$target_names[2L]) := 1 - get(self$target_names[2L])]
      as_task_surv(d, self$target_names[1L],
        self$target_names[2L],
        type = self$censtype, id = paste0(self$id, "_reverse")
      )
    },

    #' @description
    #' Returns the **proportion of censoring** for this survival task.
    #' By default, this is returned for all observations, otherwise only the
    #' specified ones (`rows`).
    #'
    #' Only designed for `"right"` and `"left"` censoring.
    #'
    #' @return `numeric()`
    cens_prop = function(rows = NULL) {
      assert_choice(self$censtype, choices = c("right", "left"))

      status = self$status(rows)
      total_censored = sum(status == 0)
      n_obs = length(status)

      total_censored / n_obs
    },

    #' @description
    #' Returns an estimated proportion of **administratively censored
    #' observations** (i.e. censored at or after a user-specified time point).
    #' Our main assumption here is that in an administratively censored dataset,
    #' the maximum censoring time is likely close to the maximum event time and
    #' so we expect higher proportion of censored subjects near the study end date.
    #'
    #' Only designed for `"right"` and `"left"` censoring.
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
      assert_choice(self$censtype, choices = c("right", "left"))
      assert_number(quantile_prob, lower = 0.8, upper = 1, null.ok = FALSE)
      assert_number(admin_time, lower = 0, null.ok = TRUE)

      times  = self$times(rows)
      status = self$status(rows)

      # Get administrative time
      if (is.null(admin_time)) {
        t_max = unname(round(quantile(times, probs = quantile_prob)))
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
    #' Only designed for `"right"` and `"left"` censoring.
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
      assert_choice(self$censtype, choices = c("right", "left"))

      status_var  = self$target_names[[2]]
      glm_summary = glm(formula = mlr3misc::formulate(lhs = status_var, rhs = "."),
                        data = self$data(cols = c(self$feature_names, status_var)),
                        family = binomial(link = "logit")) |> summary()
      # extract the p-values
      p_values = glm_summary$coefficients[, "Pr(>|z|)"]
      p_values_adj = p.adjust(p_values, method = method)
      n_coefs = length(p_values_adj) - 1 # exclude the intercept, include dummy-encoded variables
      n_signif = sum(p_values_adj[-1] <= sign_level)

      n_signif / n_coefs
    },

    #' @description
    #' Checks if the data satisfy the *proportional hazards (PH)* assumption using
    #' the Grambsch-Therneau test, `r mlr3misc::cite_bib("grambsch_1994")`.
    #' Uses [cox.zph][survival::cox.zph()].
    #' This method should be used only for **low-dimensional datasets** where
    #' the number of features is relatively small compared to the number of
    #' observations.
    #'
    #' Only designed for `"right"` and `"left"` censoring.
    #'
    #' @return `numeric()` \cr
    #' If no errors, the p-value of the global chi-square test.
    #' A p-value \eqn{< 0.05} is an indication of possible PH violation.
    prop_haz = function() {
      assert_choice(self$censtype, choices = c("right", "left"))

      cox = lrn("surv.coxph")
      cox$encapsulate = c(train = "evaluate", predict = "evaluate")
      cox$train(self)
      ok = (length(cox$errors) == 0) & (length(cox$warnings) == 0)

      # cox model didn't converge, train didn't succeed, etc
      if (!ok) stop("Error/warning during cox model fitting")

      zph_test = survival::cox.zph(fit = cox$model)
      p_value = zph_test$table["GLOBAL", "p"]

      p_value
    }
  ),

  active = list(
    #' @field censtype `character(1)`\cr
    #' Returns the type of censoring, one of `"right"`, `"left"`, `"counting"`,
    #' `"interval"`, `"interval2"` or `"mstate"`.
    #' Currently, only the `"right"`-censoring type is fully supported, the rest
    #' are experimental and the API will change in the future.
    censtype = function() {
      return(private$.censtype)
    }
  ),

  private = list(
    .censtype = character()
  )
)
