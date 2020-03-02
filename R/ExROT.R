ExROT <- function(kernel, N){

  assertDistribution(kernel)

  suppressMessages({
    if(kernel$name == "NormalKernel"){
      C = 1
      sd = 1
    } else {
      kernel = decorate(kernel, "CoreStatistics")
      sd = kernel$stdev()
      k4 = kernel$kthmoment(4,type="raw") - 3*kernel$kthmoment(2,type="raw")^2 -
        4*kernel$kthmoment(1,type="raw")*kernel$kthmoment(3,type="raw") +
        12*kernel$kthmoment(1,type="raw")^2*kernel$kthmoment(2,type="raw") -
        6*kernel$kthmoment(1,type="raw")^4
      C = 1 +
        0.3764*(k4^2/sd^8) +
        0.7292*(k4/sd^4)
    }

    return(1.0592 * sd * (C*N)^(-1/5))
  })
}
