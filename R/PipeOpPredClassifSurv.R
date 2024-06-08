
PipeOpPredClassifSurv <- R6Class(
  "PipeOpPredClassifSurv",
  inherit = PipeOpPredTransformer,

  public = list(
    initialize = function(id = "trafopred_classifsurv") {
      super$initialize(id = id,
                       input = data.table::data.table(name = c("input", "meta"), train = c("NULL", "*"), predict = c("PredictionClassif", "*")),
                       output = data.table::data.table(name = "output", train = "NULL", predict = "PredictionSurv"))
    }
  ),

  private = list(
    .train = function(input) {
      self$state = list()
      list(input)
    },

    .predict = function(input) {
      data = input[[2]]
      pred = input[[1]]
      data <- cbind(data, pred = pred$prob[, 2])

      ## convert hazards to surv as prod(1 - h(t))
      surv <- t(sapply(unique(data$id), function(i) {
        x <- cumprod((1 - data[data$id == i, "pred"]))
        x
      }))

      r <- list()
      time <- sort(unique(data$tend))
      ## coerce to distribution and crank
      r <- .surv_return(time, surv = surv)

      crank = c()
      distr = list()

      # select real tend values by only selecting the last row of each id
      # basically a slightly more complex unique()
      real_tend <- data$time2[seq_len(nrow(data)) %% sum(data$id == 1) == 0]

      ## create prediction object
      data = data %>% group_by(id) %>% filter(tend == max(tend))
      p <- PredictionSurv$new(
        row_ids = seq(nrow(data)),
        crank = r$crank, distr = r$distr,
        truth = Surv(real_tend, data[["ped_status"]]))
      ## evaluate with Harrell's C and IGS
      list(p)
    }
  )
)

task = tsk("rats")
po_discretize <- PipeOpTaskSurvClassif$new()
po_learner = po("learner", learner = lrn("classif.log_reg"), predict_type = "prob")
po_convert = PipeOpPredClassifSurv$new()
pipeline = po_discretize %>>% list(po_learner, po("nop")) %>>% po_convert

rats = tsk("rats")
# split = partition(tsk("rats"), ratio = 0.5)
rats$data(split$train)

pipeline$train(rats_train)
# rats = tsk("rats")
p = pipeline$predict(rats_test)

cox = lrn("surv.coxph")
cox$train(rats_train)
p2 = cox$predict(rats_test)

p$trafopred_classifsurv.output$score()
p2$score()

p$trafopred_classifsurv.output$truth
p2$truth

predClassif_toSurv(pred, new_data)
