#' @export
partition.TaskSurv = function(task, ratio = 0.67, stratify = TRUE, ...) { # nolint
  if (stratify) {
    task = task$clone()
    task$set_col_roles(task$target_names[2L], add_to = "stratum")
  }

  NextMethod("partition")
}
