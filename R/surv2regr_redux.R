library(mlr3pipelines)

survregr_redux = function(method = 1, graph_learner = TRUE,
                          regr_learner = "regr.featureless", distrcompose = TRUE,
                          distr_estimator = "surv.kaplan", regr_se_learner = NULL,
                          surv_learner = "surv.coxph") {

  if (method == 1) {
    gr = Graph$new()$
      add_pipeop(po("nop", phase = "predict", id = "task_surv"))$
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
      add_pipeop(po("nop", phase = "predict", id = "task_surv"))$
      add_pipeop(po("trafotask_survregr"))$
      add_pipeop(po("compose_probregr"))$
      add_pipeop(po("trafopred_regrsurv"))$
      add_edge("compose_probregr", "trafopred_regrsurv", dst_channel = "pred")$
      add_edge("task_surv", "trafopred_regrsurv", dst_channel = "task")

    if (!is.null(regr_se_learner)) {
      gr$add_pipeop(po("learner", lrn(regr_learner), id = "regr_learner"))$
        add_pipeop(po("learner", lrn(regr_se_learner, predict_type = "se"), id = "regr_se_learner"))$
        add_edge("trafotask_survregr", "regr_se_learner")$
        add_edge("regr_se_learner", "compose_probregr", dst_channel = "input_se")
    } else {
      gr$add_pipeop(po("learner", lrn(regr_learner, predict_type = "se"), id = "regr_learner"))$
        add_edge("regr_learner", "compose_probregr", dst_channel = "input_se")
    }

    gr$add_edge("trafotask_survregr", "regr_learner")$
      add_edge("regr_learner", "compose_probregr", dst_channel = "input_response")

  } else if (method == 3) {
    surv_learner = lrn(surv_learner)
    assert("lp" %in% surv_learner$predict_types)

    gr = Graph$new()$
      add_pipeop(po("nop", phase = "both", id = "task_surv_train"))$
      add_pipeop(po("nop", phase = "predict", id = "task_surv_predict"))$
      add_pipeop(po("learner_cv", surv_learner, id = "surv_learner"))$
      add_pipeop(po("trafotask_survregr", method = "reorder", target = "surv_learner.lp"))$
      add_pipeop(po("learner", lrn(regr_learner), id = "regr_learner"))$
      add_pipeop(po("trafopred_regrsurv", target_type = "lp"))$
      add_edge("surv_learner", "trafotask_survregr", dst_channel = "input")$
      add_edge("task_surv_train", "trafotask_survregr", dst_channel = "input_features")$
      add_edge("trafotask_survregr", "regr_learner")$
      add_edge("regr_learner", "trafopred_regrsurv", dst_channel = "pred")$
      add_edge("task_surv_predict", "trafopred_regrsurv", dst_channel = "task")

    if (distrcompose) {
      gr$add_pipeop(po("learner", lrn(distr_estimator), id = "distr_estimator"))$
        add_pipeop(po("compose_distr"))$
        add_edge("trafopred_regrsurv", dst_id = "compose_distr", dst_channel = "pred")$
        add_edge("distr_estimator", dst_id = "compose_distr", dst_channel = "base")
    }
  }

  if (graph_learner) {
    return(GraphLearner$new(gr))
  } else {
    return(gr)
  }
}

