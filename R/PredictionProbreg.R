#' PredictionProbreg = R6::R6Class("PredictionProbreg", inherit = Prediction, cloneable = FALSE)
#' PredictionProbreg$set("public", "initialize", function(task = NULL, row_ids = task$row_ids,
#' truth = task$truth(),
#'                                                        prob = NULL) {
#'   self$data$row_ids = assert_row_ids(row_ids)
#'   n = length(row_ids)
#'   self$data$truth = checkmate::assert_numeric(truth, len = n, null.ok = TRUE)
#'   self$data$prob = distr6::assertDistribution(prob)
#'   self$task_type = "probreg"
#' })
#' PredictionProbreg$set("active","prob",function() {
#'   self$data$prob %??% rep(NA_real_, length(self$data$row_ids))
#' })
#' PredictionProbreg$set("active","missing",function() {
#'   miss = logical(length(self$data$row_ids))
#'   if (!is.null(self$data$prob))
#'     miss = miss | is.na(self$data$prob)
#'
#'   self$data$row_ids[miss]
#' })
#' PredictionProbreg$set("public","print",function() {
#'   dt = as.data.table(self)
#'   dt$prob = lapply(dt$prob, strprint)
#'   print(dt)
#' })
#'
#' #' export
#' as.data.table.PredictionProbreg = function(x, ...) {
#'   data = x$data
#'   if (is.null(data$row_ids)) {
#'     return(data.table::data.table())
#'   }
#'
#'   prob = data$prob$wrappedModels()
#'
#'   data.table::data.table(row_id = data$row_ids, truth = data$truth, prob = prob)
#' }
#'
#'
#' #' export
#' c.PredictionProbreg = function(..., keep_duplicates = TRUE) {
#'
#'   dots = list(...)
#'   assert_list(dots, "PredictionProbreg")
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
#'   PredictionProbreg$new(row_ids = x$row_ids, truth = x$truth, prob = x$prob)
#' }
