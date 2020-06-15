context("MeasureSurvHarrellC")

test_that("missing surv", {
  lung2 = load_dataset("lung", "survival")
  lung2$time[1] = NA
  t = TaskSurv$new("s", backend = lung2, time = "time", event = "status")
  anyMissing(t$truth())
  expect_true(is.na(cindex(t$truth(), 1)))
})
