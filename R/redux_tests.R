task = tgen("simsurv")$generate(20)

learn1a = survregr_redux(method = 1, graph_learner=FALSE, distrcompose = FALSE)
plot(learn1a, html = TRUE)
learn1a = GraphLearner$new(learn1a)
learn1a$train(task)
learn1a$predict(task)

learn1b = survregr_redux(method = 1, graph_learner=FALSE, distrcompose = TRUE)
plot(learn1b, html = TRUE)
learn1b = GraphLearner$new(learn1b)
learn1b$train(task)
learn1b$predict(task)

learn2a = survregr_redux(method = 2, graph_learner=F)
plot(learn2a, html = TRUE)
learn2a = GraphLearner$new(learn2a)
learn2a$train(task)
learn2a$predict(task)

learn2b = survregr_redux(method = 2, graph_learner=F, regr_se_learner="regr.featureless")
plot(learn2b, html = TRUE)
learn2b = GraphLearner$new(learn2b)
learn2b$train(task)
learn2b$predict(task)

learn3a = survregr_redux(method = 3, graph_learner=F, distrcompose = F)
plot(learn3a, html = TRUE)
learn3a = GraphLearner$new(learn3a)
learn3a$train(task)
learn3a$predict(task)


learn3b = survregr_redux(method = 3, graph_learner=F, distrcompose = T)
plot(learn3b, html = TRUE)
learn3b = GraphLearner$new(learn3b)
learn3b$train(task)
learn3b$predict(task)

