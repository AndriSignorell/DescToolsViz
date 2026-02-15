
#' Combined Plot of a Time Series and Its ACF and PACF 
#' 
#' Combined plot of a time Series and its autocorrelation and partial
#' autocorrelation 
#' 
#' plotTimeSeries plots a combination of the time series and its autocorrelation and
#' partial autocorrelation. 
#' 
#' @aliases plotTimeSeries
#' 
#' @param x univariate time series. 
#' @param lag.max integer. Defines the number of lags to be displayed. The
#' default is 10 * log10(length(series)). 
#' @param main an overall title for the plot
#' @param ylab a title for the y axis: see \code{\link{title}}. 
#' @param \dots the dots are passed to the plot command. 
#' 
#' @author Andri Signorell <andri@@signorell.net>, rewritten based on ideas of M.Huerzeler
#' @seealso \code{\link{ts}}
#' @keywords hplot
#' @examples
#' 
#' plotTimeSeries(AirPassengers)
 

#' @export
plotTimeSeries <- function (x, lag.max = 10 * log10(length(x)), 
                            ylab = NULL,
                            main=NULL,  ...) {

    if (!is.null(dim(x))) 
      stop("plotTimeserie() requires univariate time series")
    
    
    .withGraphicsState({
      
      layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE),
             heights = c(2,1))
      
      # oben
      par(mar = c(3,4,3,1))
      plot(x, main = main, xlab="", 
           ylab = ylab %||% deparse(substitute(x)))
      
      # unten links
      .plotACF(acf(x, plot=FALSE), zero = "set")
      
      # unten rechts
      par(mar = c(4,4,0,1))
      .plotACF(pacf(x, plot=FALSE), zero = "add")
      
    })
    
    invisible(x)
    
}


# == internal helper functions =========================================

.plotACF <- function(x, main=NULL, zero=c("set", "add")){
  
    par(mar = c(4,4,0,1), mgp=c(2.5,1,0))
    
    # set first value to 0 instead of 1
    if(zero == "set")
      # needed for acf
      x$acf[,,1][1] <- 0
    
    else if(zero == "add"){
      # used for partial acaf
      x$acf <- abind::abind(array(0, dim = c(1,1,1)), 
                            x$acf, along = 1)
      x$lag <- abind::abind(array(0, dim = c(1,1,1)), 
                            x$lag, along = 1)
    }
    
    plot(x, main=main, xaxt="n", xlab="Lag k")
    
    # special x-axis
    i <- seq(0, length(x$lag), by=5)
    axis(1, at=c(0, x$lag[i]), labels=c(0, seq(x$lag)[i]))
    rug(x$lag, ticksize = -0.03)

}

  