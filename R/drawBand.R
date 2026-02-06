
#' Draw Confidence Band 
#' 
#' Draw a band using a simple syntax. Just a wrapper for the function
#' \code{polygon()} typically used to draw confidence bands. 
#' 
#' 
#' @param x a vector or a matrix with x coordinates for the band. If x is given
#' as matrix it must be a \eqn{2 \times n}{2 x n} matrix and the second column
#' will be reversed. x will be recyled in the case y is a 2dimensional matrix.
#' @param y a vector or a matrix with y coordinates for the band. If y is given
#' as matrix it must be a \eqn{2 \times n}{2 x n} matrix and the second column
#' will be reversed. y will be recyled in the case x is a 2dimensional matrix.
#' @param col the color of the band. 
#' @param border the border color of the band. 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{polygon}} 
#' @keywords aplot
#' 
#' @examples
#' 
#' set.seed(18)
#' x <- rnorm(15)
#' y <- x + rnorm(15)
#' 
#' new <- seq(-3, 3, 0.5)
#' pred.w.plim <- predict(lm(y ~ x), newdata=data.frame(x=new), interval="prediction")
#' pred.w.clim <- predict(lm(y ~ x), newdata=data.frame(x=new), interval="confidence")
#' 
#' plot(y ~ x)
#' drawBand(y = c(pred.w.plim[,2], rev(pred.w.plim[,3])),
#'   x=c(new, rev(new)), col= alpha("grey90", 0.5))
#' 
#' # passing y as matrix interface allows more intuitive arguments
#' drawBand(y = pred.w.clim[, 2:3],
#'          x = new, col= alpha("grey80", 0.5))
#' 
#' abline(lm(y~x), col="brown")
#' 

 
#' @export
drawBand <- function(x, y, col = alpha("grey", 0.5), border = NA) {
  
  # accept matrices but then only n x y
  if(!identical(dim(y), dim(x))){
    x <- as.matrix(x)
    y <- as.matrix(y)
    
    if(dim(x)[2] == 1 && dim(y)[2] == 2)
      x <- x[, c(1,1)]
    else if(dim(x)[2] == 2 && dim(y)[2] == 1)
      y <- y[, c(1,1)]
    else
      stop("incompatible dimensions for matrices x and y")
    
    x <- c(x[,1], rev(x[,2]))
    y <- c(y[,1], rev(y[,2]))
    
  }
  
  # adds a band to a plot, normally used for plotting confidence bands
  polygon(x=x, y=y, col = col, border = border)
  
}


