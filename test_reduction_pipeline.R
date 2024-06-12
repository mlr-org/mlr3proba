
taskSurv_toClassif = function(task, cut = NULL) {
  assert_task(task, task_type = "surv")
  assert_numeric(cut, null.ok = TRUE, lower = 0)
  cut = seq(0, max(tsk("rats")$data(, "time")), length.out = 5)
  time = task$target_names[1]
  event = task$target_names[2]

  formula = sprintf("Surv(%s, %s) ~ .", time, event)

  # TODO: do without pammtools
  long_data = as.data.table(pammtools::as_ped(as.formula(formula), data = task$data(), cut = cut))
  long_data$ped_status = factor(long_data$ped_status)

  long_data[, c("offset", "tstart", "interval") := NULL]

  task = TaskClassif$new(paste0(task$id, "_disc"), long_data, target = "ped_status")
  task$set_col_roles("id", roles = "name")

  return(task)
}


predClassif_toSurv = function(pred, new_data) {
  data = cbind(new_data, pred = pred$prob[, 2])

  ## convert hazards to surv as prod(1 - h(t))
  surv = t(sapply(unique(data$id), function(i) {
    x = cumprod((1 - data[data$id == i, "pred"]))
    x
  }))

  r = list()
  time = sort(unique(new_data$tend))
  ## coerce to distribution and crank
  r = .surv_return(time, surv = surv)

  crank = c()
  distr = list()

  # real tend values
  real_tend = new_data$time2[seq_len(nrow(new_data)) %% sum(new_data$id == 1) == 0]

  ## create prediction object
  data = data |> dplyr::group_by(id) |>  dplyr::filter(tend == max(tend))
  p = PredictionSurv$new(
    row_ids = seq(nrow(data)),
    crank = r$crank, distr = r$distr,
    truth = Surv(real_tend, data[["ped_status"]]))
  ## evaluate with Harrell's C and IGS
  out = list()
  out$H_C = as.numeric(p$score())
  out$IGS = as.numeric(p$score(msr("surv.graf", proper = TRUE)))

  out
}

# set.seed(4)
rats = tsk("rats")
split = partition(tsk("rats"), ratio = 0.9)
rats_train = rats$filter(split$train)
rats_train_long = taskSurv_toClassif(rats_train)
rats = tsk("rats")
rats_test = rats$filter(split$test)
rats = tsk("rats")

maximum = max(rats_train$data(, "time"))
data2 = as.data.frame(rats_test$data())  |> dplyr::mutate(time2 = time, time = maximum)

new_data = pammtools::as_ped(pammtools::as_ped(data = as.data.frame(rats_train$data()), Surv(time, status)~.,
                         cut = seq(0, max(tsk("rats")$data(, "time")), length.out = 5)), newdata = data2)
new_data$ped_status = factor(new_data$ped_status)

################################################################################
max(rats_train_long$data(,"tend")) # if not == 104 -> error
new_data$ped_status |> unique()
rats_train$data()
################################################################################

# train and predict
learner = mlr3learners::LearnerClassifLogReg$new()
learner$predict_type = "prob"
pred = learner$train(rats_train_long)$predict_newdata(new_data)
predClassif_toSurv(pred, new_data)


################################################################################

task = tsk("rats")
po_discretize = PipeOpTaskSurvClassif$new()
po_learner = mlr3pipelines::po("learner",
                               learner = mlr3learners::LearnerClassifLogReg$new(),
                               predict_type = "prob")
po_convert = PipeOpPredClassifSurv$new()
pipeline = po_discretize |>
  mlr3pipelines::`%>>%`(list(po_learner, mlr3pipelines::po("nop"))) |>
  mlr3pipelines::`%>>%`(po_convert)

rats$data(split$train)

pipeline$train(rats_train)
p = pipeline$predict(rats_test)

cox = lrn("surv.coxph")
cox$train(rats_train)
p2 = cox$predict(rats_test)

p$trafopred_classifsurv.output$score()
p2$score()

p$trafopred_classifsurv.output$truth
p2$truth

predClassif_toSurv(pred, new_data)

################################################################################

task = tsk("rats")
po_discretize = PipeOpTaskSurvClassif$new()
# po_discretize = PipeOpTaskSurvClassif$new(cut = seq(0, max(rats$time), length.out = 100))
# po_discretize = mlr3pipelines::po("trafotask_survclassif", cut = seq(0, max(rats$time), length.out = 100))
po_discretize = mlr3pipelines::po("trafotask_survclassif", cut = 5)
po_discretize$train(list(task))
po_discretize$predict(list(task))

po_discretize = PipeOpTaskSurvClassif$new()
learner_po = mlr3pipelines::po("learner",
                               learner = mlr3learners::LearnerClassifLogReg$new(),
                               predict_type = "prob")
pipeline = po_discretize |> mlr3pipelines::`%>>%`(list(learner_po, mlr3pipelines::po("nop")))


pipeline$train(task)
pipeline$predict(task)

