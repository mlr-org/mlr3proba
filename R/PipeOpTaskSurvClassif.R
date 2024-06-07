PipeOpTaskSurvClassif <- R6Class(
  "PipeOpTaskSurvClassif",
  inherit = PipeOpTaskTransformer,

  public = list(
    initialize = function(id = "trafotask_survclassif") {
      super$initialize(id = id,
                       input = data.table::data.table(name = "input", train = "TaskSurv", predict = "TaskSurv"),
                       output = data.table::data.table(name = c("output", "meta"), train = c("TaskClassif", "*"), predict = c("TaskClassif", "*")))
    }
  ),

  private = list(
    .train = function(task, cut = NULL) {
      cut = NULL
      task = task[[1]]
      data = task

      assert_task(task, task_type = "surv")
      assert_numeric(cut, null.ok = TRUE, lower = 0)

      time = task$target_names[1]
      event = task$target_names[2]

      formula = sprintf("Surv(%s, %s) ~ .", time, event)

      # TODO: do without pammtools
      long_data = pammtools::as_ped(as.formula(formula), data = task$data(), cut = cut)
      self$state$long_data = long_data
      long_data = as.data.table(long_data)
      long_data$ped_status = factor(long_data$ped_status)

      # remove offset, tstart, interval for dataframe long_data
      long_data[, c("offset", "tstart", "interval") := NULL]

      task = TaskClassif$new(paste0(task$id, "_disc"), long_data, target = "ped_status")
      task$set_col_roles("id", roles = "name")

      list(task, NULL)
    },

    .predict = function(task) {
      task = task[[1]]
      data2 = task$data()

      maximum <- max(self$state$long_data$tend)
      time = data2$time
      data2$time <- maximum
      data2$time2 <- time

      new_data = as_ped(self$state$long_data, newdata = data2)
      new_data$ped_status = factor(new_data$ped_status)
      list(TaskClassif$new(paste0(task$id, "_disc"), new_data, target = "ped_status"),
           new_data)
    }
  )
)

task = tsk("rats")
po_discretize = PipeOpTaskSurvClassif$new()
po_discretize$train(list(task))
po_discretize$predict(list(task))

po_discretize <- PipeOpTaskSurvClassif$new()
learner_po = po("learner", learner = lrn("classif.ranger"), predict_type = "prob")
pipeline = po_discretize %>>% list(learner_po, po("nop"))


pipeline$train(task)
pipeline$predict(task)

