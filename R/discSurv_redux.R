library(mlr3verse)
library(mlr3pipelines)
library(mlr3proba)
library(checkmate)
library(survival)
library(pammtools)
library(R6)

taskSurv_toClassif = function(task, cut = NULL) {
  assert_task(task, task_type = "surv")
  assert_numeric(cut, null.ok = TRUE, lower = 0)

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
  data <- cbind(new_data, pred = pred$prob[, 2])

  ## convert hazards to surv as prod(1 - h(t))
  surv <- t(sapply(unique(data$id), function(i) {
    x <- cumprod((1 - data[data$id == i, "pred"]))
    x
  }))

  r <- list()
  time <- sort(unique(new_data$tend))
  ## coerce to distribution and crank
  r <- .surv_return(time, surv = surv)

  crank = c()
  distr = list()

  # real tend values
  real_tend <- new_data$time2[seq_len(nrow(new_data)) %% sum(new_data$id == 1) == 0]

  ## create prediction object
  data = data %>% group_by(id) %>% filter(tend == max(tend))
  p <- PredictionSurv$new(
    row_ids = seq(nrow(data)),
    crank = r$crank, distr = r$distr,
    truth = Surv(real_tend, data[["ped_status"]]))
  ## evaluate with Harrell's C and IGS
  out <- list()
  out$H_C <- as.numeric(p$score())
  out$IGS <- as.numeric(p$score(msr("surv.graf", proper = TRUE)))

  out
}

# set.seed(4)
rats = tsk("rats")
split = partition(tsk("rats"), ratio = 0.9)
rats_train = rats$filter(split$train)
rats_train_long = taskSurv_toClassif(rats_train)
rats = tsk("rats")
rats_test = rats$filter(split$test)
rats_test_long = taskSurv_toClassif(rats_test)
rats = tsk("rats")

maximum <- max(rats_train$data(, "time"))
data2 <- as.data.frame(rats_test$data())  |> mutate(time2 = time, time = maximum)

new_data = as_ped(as_ped(data = as.data.frame(rats_train$data()), Surv(time, status)~.,
                         cut = NULL), newdata = data2)
new_data$ped_status = factor(new_data$ped_status)

################################################################################
max(rats_train_long$data(,"tend")) # if not == 104 -> error
new_data$ped_status |> unique()
rats_train$data()
################################################################################

# train and predict
pred <- lrn("classif.log_reg", predict_type = "prob")$train(rats_train_long)$predict_newdata(new_data)
predClassif_toSurv(pred, new_data)
