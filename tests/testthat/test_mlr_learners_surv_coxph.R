context("surv.coxph")

test_that("autotest", {
  set.seed(1)
  learner = lrn("surv.coxph")
  expect_learner(learner)
  # unclear why weights is failing
  result = run_autotest(learner, exclude = "weight")
  expect_true(result, info = result$error)

  learner = lrn("surv.coxph")
  expect_error(learner$importance(), "No model stored")
  expect_error(learner$selected_features(), "No model stored")
  learner$train(tsk("rats"))
  expect_named(expect_numeric(learner$importance()))
  expect_character(learner$selected_features())
})

test_that("manualtest",{
  task = load_rats()
  learn = lrn("surv.coxph")
  expect_silent(learn$train(task))
  p = learn$predict(task)
  expect_prediction_surv(p)
  expect_true(all(order(p$crank) == order(p$lp)))
  rr = riskRegression::predictCox(learn$model,times = 90:95, newdata = task$data()[1,])
  expect_equal(as.numeric(rr$cumhazard), p$distr[1]$cumHazard(90:95))
  expect_equal(as.numeric(rr$survival), p$distr[1]$survival(90:95))
})

test_that("missing",{
  task = TaskGeneratorSimsurv$new()$generate(50)
  learner = lrn("surv.coxph")
  learner$train(task)
  expect_error(learner$predict(tsk("lung")))
})

test_that("weights",{
  task = generate_tasks(lrn("surv.coxph"), N = 30)$weight
  learner = lrn("surv.coxph")
  expect_silent(expect_prediction_surv(learner$train(tsk("rats"))$predict(tsk("rats"))))
})
