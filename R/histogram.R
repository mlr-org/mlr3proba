# Description: Finding the pdf and cdf using histogram using the hist function in R

# Arguments
# 1. data to estimate the density
# 2. numbin: the size of each bins. Can be a single numberr, vector, a function (see hist)


.histogram = function(dat, breaks = "Sturges") {
  fit = graphics::hist(x = dat, breaks = breaks, include.lowest = TRUE, plot = FALSE, right = FALSE)

  pdf = function(x) {} #nolint
  body(pdf) = substitute({
    pdf = numeric(length(x))
    ind = x >= min(Intervals) & x <= max(Intervals)
    pdf[ind] = f[findInterval(x[ind], Intervals, left.open = F, rightmost.closed = T)]
    return(pdf)
  }, list(f = fit$density, Intervals = fit$breaks))

  cdf = function(x) {} #nolint
  body(cdf) = substitute({
    sapply(x, function(x) .histogram_cdf(val = x, Intervals = Intervals, pdf = pdf, counts = counts))
  }, list(counts = fit$counts, pdf = fit$density, Intervals = fit$breaks))

  list(
    distr = distr6::Distribution$new(
      name = "Histogram Estimator",
      short_name = "Histogram",
      pdf = pdf, cdf = cdf,
      type = set6::Reals$new(),
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

.histogram_cdf = function(val, Intervals, pdf, counts) {
  if (val < min(Intervals)) {
    return(0)
  } else if (val >= max(Intervals)) {
    return(1)
  } else {
    ind = findInterval(val, Intervals, left.open = F, rightmost.closed = T)
    # finding the index of the breaks for val
    part_cdf = cumsum(counts) / sum(counts)
    # find the area of the bin up to LHS val

    # if ind == 1, it means that its the first Interval,
    if (ind == 1) {
      return((val - Intervals[ind]) * pdf[ind])
    } else {
      return(part_cdf[ind - 1] + (val - Intervals[ind]) * pdf[ind])
    }
  }
}
