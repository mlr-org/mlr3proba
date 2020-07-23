library(mlr3pipelines)

survregr_redux = function(method = 1, regr_learner = "regr.featureless", distrcompose = TRUE,
                          distr_estimator = "surv.kaplan", surv_learner = "surv.coxph") {

  if (method == 1) {
    gr = Graph$new()$
      add_pipeop(po("nop", id = "task_surv"))$
      add_pipeop(po("trafotask_survregr"))$
      add_pipeop(po("learner", lrn(regr_learner), id = "regr_learner"))$
      add_pipeop(po("trafopred_regrsurv"))$
      add_edge("trafotask_survregr", "regr_learner")$
      add_edge("regr_learner", "trafopred_regrsurv", dst_channel = "pred")$
      add_edge("task_surv", "trafopred_regrsurv", dst_channel = "task")

    if (distrcompose) {
      gr$add_pipeop(po("learner", lrn(distr_estimator), id = "distr_estimator"))$
        add_pipeop(po("compose_distr"))$
        add_edge("trafopred_regrsurv", dst_id = "compose_distr", dst_channel = "pred")$
        add_edge("distr_estimator", dst_id = "compose_distr", dst_channel = "base")
    }

  } else if (method == 2) {
    gr = Graph$new()$
      add_pipeop(po("nop", id = "task_surv"))$
      add_pipeop(po("trafotask_survregr"))$
      add_pipeop(po("learner", lrn(regr_learner), id = "regr_learner"))$
      add_pipeop(po("compose_probregr"))$
      add_pipeop(po("trafopred_regrsurv"))$
      add_edge("trafotask_survregr", "regr_learner")$
      add_edge("regr_learner", "compose_probregr")$
      add_edge("compose_probregr", "trafopred_regrsurv", dst_channel = "pred")$
      add_edge("task_surv", "trafopred_regrsurv", dst_channel = "task")
  } else if (method == 3) {
    surv_learner = lrn(surv_learner)
    assert("lp" %in% surv_learner$predict_types)

    gr = Graph$new()$
      add_pipeop(po("nop", id = "task_surv"))$
      add_pipeop(po("learner", surv_learner, id = "surv_learner"))$
      add_pipeop(po("trafopredtask_survregr", target = "lp"))$
      add_pipeop(po("learner", lrn(regr_learner), id = "regr_learner"))$
      add_pipeop(po("trafopred_regrsurv"))$
      add_edge("surv_learner", "trafopredtask_survregr", dst_channel = "pred")$
      add_edge("task_surv", "trafopredtask_survregr", dst_channel = "task")$
      add_edge("trafopredtask_survregr", "regr_learner")$
      add_edge("regr_learner", "trafopred_regrsurv", dst_channel = "pred")$
      add_edge("task_surv", "trafopred_regrsurv", dst_channel = "task")

    if (distrcompose) {
      gr$add_pipeop(po("learner", lrn(distr_estimator), id = "distr_estimator"))$
        add_pipeop(po("compose_distr"))$
        add_edge("trafopred_regrsurv", dst_id = "compose_distr", dst_channel = "pred")$
        add_edge("base_estimator", dst_id = "compose_distr", dst_channel = "base")
    }
  }

  GraphLearner$new(gr)
}
