.DensGenKern <- function(x, xgridsize = 100, range.x, xbandwidth = 1){
    return(sapply(range.x, function(y) GenKern::KernSec(x = x, xgridsize = xgridsize, xbandwidth = xbandwidth,
                                            range.x = y)$yden/xgridsize))
}
