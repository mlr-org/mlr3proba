#' PredictionDensity = R6::R6Class("PredictionDensity", inherit = Prediction, cloneable = FALSE)
#' PredictionDensity$set("public", "initialize", function(task = NULL, row_ids = task$row_ids, truth = task$truth(),
#'                                                        prob = NULL) {
#'   self$data$row_ids = assert_row_ids(row_ids)
#'   n = length(row_ids)
#'   self$data$truth = checkmate::assert_numeric(truth, len = n, null.ok = TRUE)
#'   self$data$prob = checkmate::assert_numeric(prob, len = n, any.missing = FALSE, null.ok = TRUE)
#'   self$task_type = "density"
#' })
#' PredictionDensity$set("active","prob",function(){
#'   self$data$prob %??% rep(NA_real_, length(self$data$row_ids))
#' })
#' PredictionDensity$set("active","missing",function(){
#'   miss = logical(length(self$data$row_ids))
#'   if (!is.null(self$data$prob))
#'     miss = miss | is.na(self$data$prob)
#'
#'   self$data$row_ids[miss]
#' })
#'
#' #' export
#' as.data.table.PredictionDensity = function(x, ...) {
#'   data = x$data
#'   if (is.null(data$row_ids)) {
#'     return(data.table::data.table())
#'   }
#'   data.table::data.table(row_id = data$row_ids, truth = data$truth, prob = data$prob)
#' }
#'
#'
#' #' export
#' c.PredictionDensity = function(..., keep_duplicates = TRUE) {
#'
#'   dots = list(...)
#'   assert_list(dots, "PredictionDensity")
#'   assert_flag(keep_duplicates)
#'
#'   x = map_dtr(dots, function(p) {
#'     list(row_ids = p$data$row_ids, truth = p$data$truth, prob = p$data$prob)
#'   }, .fill = FALSE)
#'
#'   if (!keep_duplicates) {
#'     keep = !duplicated(x$row_ids, fromLast = TRUE)
#'     x = x[keep]
#'   }
#'
#'   PredictionDensity$new(row_ids = x$row_ids, truth = x$truth, prob = x$prob)
#' }
