TaskSurv = R6::R6Class("TaskSurv",
                       inherit = TaskSupervised,
                       public = list(
                         initialize = function(id, backend, time, status) {
                           super$initialize(id = id, task_type = "surv", backend = backend, target = c(time, status))

                           status = self$data(cols = status)[[1L]]
                           if (!is.logical(status)) {
                             assert_integerish(status, lower = 0, upper = 1)
                           }
                         },

                         truth = function(row_ids = NULL) {
                           tn = self$target_names
                           d = self$data(row_ids, cols = self$target_names)
                           survival::Surv(d[[tn[1L]]], as.logical(d[[tn[2L]]]))
                         },

                         formula = function(rhs = NULL) {
                           tn = self$target_names
                           lhs = sprintf("Surv(%s, %s)", tn[1L], tn[2L])
                           formulate(lhs, rhs %??% ".", env = getNamespace("survival"))
                         },

                         survfit = function(strata = character()) {
                           assert_character(strata, any.missing = FALSE)
                           f = self$formula(rhs = strata)
                           vars = unique(unlist(extract_vars(f)))
                           survfit(f, self$data(cols = vars))
                         }
                       )
)
