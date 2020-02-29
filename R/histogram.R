#Histogram function
#-------------------

#Description: Finding the pdf and cdf using histogram using the hist function in R

#Arguments
#1. data to estimate the density
#2. numbin: the size of each bins. Can be a single numberr, vector, a function (see hist)


.histogram <- function(dat, breaks = "Sturges"){
  fit <- graphics::hist(x = dat, breaks = breaks, include.lowest = TRUE, plot = FALSE, right = FALSE)

  pdf = function(x1){}
  body(pdf) = substitute({
    f[findInterval(x1, Intervals, left.open = F, rightmost.closed = T)]
   }, list(f = fit$density, Intervals = fit$breaks))

  cdf = function(x1){}
  body(cdf) = substitute({
      sapply(x1, function(x) .histogram_cdf(val = x, Intervals = Intervals, pdf = pdf, counts = counts))
  }, list(counts = fit$counts, pdf = fit$density, Intervals = fit$breaks))

  list(distr = distr6::Distribution$new(name = "Histogram Estimator",
                           short_name = "Histogram",
                           pdf = pdf, cdf = cdf,
                           support = set6::Interval$new(min(fit$breaks), max(fit$breaks))),
       hist = fit)
}



# Description: Compute the cdf of a histogram using the density and the
#              relative intervals. The lower limit of the cdf must
#              always be the lowest limit of the histogram. To find the
#              cdf of a histogram between the values must compute cdf twice
#              and substract.

# Arguments:
# 1. val: component of Intervval which the upper limit belong to
# 2. Intervals: The intervals/break of the histogram. A vector
# 3. Pdf: pdf for each interval. a vector

.histogram_cdf <- function(val, Intervals, pdf, counts){

  ind <- findInterval(val, Intervals, left.open = F, rightmost.closed = T)
  #finding the index of the breaks for val
  part_cdf <- cumsum(counts)/sum(counts)
  # find the area of the bin up to LHS val

  # if ind == 1, it means that its the first Interval,
  if(ind == 1)
    return((val - Intervals[ind])*pdf[ind])
  else
    return(part_cdf[ind-1] + (val - Intervals[ind])*pdf[ind])
}
