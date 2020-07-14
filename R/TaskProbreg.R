# TaskProbreg = R6::R6Class("TaskProbreg", inherit = TaskSupervised)
# TaskProbreg$set("public","initialize", function(id, backend, target) {
#                        checkmate::assert_string(target)
#                        super$initialize(id = id, task_type = "probreg", backend = backend,
#                        target = target)
#                        type = subset(self$col_info, id == target, "type")
#                        if (!(type %in% c("integer", "numeric"))) {
#                          stopf("Target column '%s' must be numeric", target)
#                        }
#                      })
# TaskProbreg$set("public","truth",function(rows = NULL) {
#                        super$truth(rows)[[1L]]
#                      })
