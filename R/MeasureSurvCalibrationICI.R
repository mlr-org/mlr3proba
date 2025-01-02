# key reference: Austin, Harrell & Klaveren (2020) https://pubmed.ncbi.nlm.nih.gov/32548928/
# This method is also recommended by the recent BMJ article: https://www.bmj.com/content/384/bmj-2023-074820
# below implement the hazard regression from polspline package. Alternative method uses simple cox regression. Cox regression is somewhat less desirable as one needs to select number of knots, and the PH assumption.

# library(R6)

MeasureSurvICI = R6Class("MeasureSurvICI", inherit = MeasureSurv,
      public = list(
               #' @description Creates a new instance of this [R6][R6::R6Class] class.
                initialize = function( ) {
                  ps = ps(tm = p_dbl(0, Inf),
                          plot=p_lgl(default = FALSE))  # T/F indicator for plot
                  ps$values = list(plot=FALSE )
                super$initialize(id = "surv.ICI",
                                 range = c(0, Inf),
                                 packages = c('polspline', 'ggplot2'),
                                 minimize = TRUE,
                                 predict_type = "distr",
                                 label = "Integrated Calibration Index",
                                 man = "mlr3proba::mlr_measures_surv.ICI",
                                 param_set = ps)}
                                  ),
             
      private = list(
        .score = function(prediction, ...) {
          ps = self$param_set$values
          tm = ps$tm
        
          #event prob at specified tm
          pred = t(prediction$distr$cdf(tm)) # model predicted event prob
          pred.cll = log(-log(1-pred))
          truth = prediction$truth # get data on event time and type
          cali.hare <- polspline::hare(data=truth[,1], delta = truth[,2], 
                           cov = as.matrix(pred.cll))
          # rhs # This returns entire dataset
          
          #ICI
          ob.hare=polspline::phare(tm, pred.cll, cali.hare) 
          ICI= mean(abs(ob.hare - pred))
           
          #plot
          if (ps$plot==TRUE) {   
          pred.grid = seq(quantile(pred, probs=0.01),
                          quantile(pred, probs=0.99),
                          length=100)  # predicted event prob for plotting
          pred.grid.cll=log(-log(1-pred.grid))
          obs.grid=polspline::phare(tm, pred.grid.cll, cali.hare)  # get observed prob from hare model
          cali=data.frame(cbind(predicted=pred.grid, observed=obs.grid))
          
          # plot elements
          time_pred=paste0("time: (", tm, ")")
          y_lab=paste("Observed event prob at ", time_pred, sep="")
          x_lab=paste("Predicted event prob at ", time_pred, sep="")
          # name= paste0(learner$id, " fold ", iteration)
          
          
          p <- ggplot2::ggplot(cali, aes(x=predicted, y=observed))+
            geom_line() +
            geom_abline(slope = 1, intercept=0, alpha=0.5, linetype='dashed') +
            ylab(y_lab) +  scale_y_continuous(limits=c(0,1)) +
            xlab(x_lab) +  scale_x_continuous(limits = c(0,1)) +
            theme_bw() + ggtitle('TODO:(mlr3 benchmark) title') +
            theme(legend.position = 'top')}
        
          
          if (ps$plot==FALSE) {
            return(ICI)
          } else {
            print(p)
            # cali_list=list()
            # return(cali_list)
            }
        }
      ))

mlr3::mlr_measures$add("surv.ICI", MeasureSurvICI)