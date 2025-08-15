lg = lgr::get_logger("mlr3")
old_threshold = lg$threshold
lg$set_threshold("warn")

mirai::daemons(0, .compute = "mlr3_encapsulation")
mirai::daemons(1, .compute = "mlr3_encapsulation")
