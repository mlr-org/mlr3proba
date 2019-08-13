TaskDensity <- R6::R6Class("TaskDensity", inherit = TaskSupervised)
TaskDensity$set("public","initialize", function(id, backend, target) {
                       checkmate::assert_string(target)
                       super$initialize(id = id, task_type = "regr", backend = backend, target = target)

                       type = self$col_info[id == target]$type
                       if (!(type %in% c("integer", "numeric"))) {
                         stopf("Target column '%s' must be numeric", target)
                       }
                     })
TaskDensity$set("public","truth",function(row_ids = NULL) {
                       super$truth(row_ids)[[1L]]
                     })
