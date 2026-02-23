
' Produce a shaded Curve 
#' 
#' Sometimes the area under a density curve has to be color shaded, for
#' instance to illustrate a p-value or a specific region under the normal
#' curve. This function draws a curve corresponding to a function over the
#' interval \code{[from, to]}. It can plot also an expression in the variable
#' \code{xname}, default \code{x}.
#' 
#' Useful for shading the area under a curve as often needed for explaining
#' significance tests. 
#' 
#' @param expr the name of a function, or a \code{\link{call}} or an
#' \code{\link{expression}} written as a function of \code{x} which will
#' evaluate to an object of the same length as \code{x}. 
#' @param col color to fill or shade the shape with. The default is taken from
#' \code{par("fg")}. 
#' @param breaks numeric, a vector giving the breakpoints between the distinct
#' areas to be shaded differently. Should be finite as there are no plots with
#' infinite limits. 
#' @param density the density of the lines as needed in polygon. 
#' @param n integer; the number of x values at which to evaluate. Default is
#' 101. 
#' @param xname character string giving the name to be used for the x axis.
#' @param \dots the dots are passed on to \code{\link{polygon}}. 
#' 
#' @return A list with components \code{x} and \code{y} of the points that were
#' drawn is returned invisibly. 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{polygon}}, \code{\link{curve}} 
#' 
#' @keywords aplot
#' @examples
#' 
#' curve(dt(x, df=5), xlim=c(-6,6),
#'       main=paste("Student t-Distribution Probability Density Function, df = ", 5, ")", sep=""),
#'       type="n", las=1, ylab="probability", xlab="t")
#' 
#' shade(dt(x, df=5), breaks=c(-6, qt(0.025, df=5), qt(0.975, df=5), 6),
#'       col=c("deeppink4", "skyblue3"), density=c(20, 7))
#' 

#' @export
shade <- function(expr, col=par("fg"), breaks, density=10, n=101, xname = "x", ...) {
  
  sexpr <- substitute(expr)
  
  if (is.name(sexpr)) {
    expr <- call(as.character(sexpr), as.name(xname))
  } else {
    if (!((is.call(sexpr) || is.expression(sexpr)) && xname %in%
          all.vars(sexpr)))
      stop(gettextf("'expr' must be a function, or a call or an expression containing '%s'",
                    xname), domain = NA)
    expr <- sexpr
  }
  
  
  .shade <- function (col, from = NULL, to = NULL, density, n = 101, ...) {
    
    x <- seq(from, to, length.out=n)
    xval <- c(from, x, to)
    
    ll <- list(x = x)
    names(ll) <- xname
    # Calculates the function for given xval
    yval <- c(0, eval(expr, envir = ll, enclos = parent.frame(n=2)), 0)
    if (length(yval) != length(xval))
      stop("'expr' did not evaluate to an object of length 'n'")
    
    polygon(xval, yval, col=col, density=density, ...)
    
    invisible(list(x = xval, y = yval))
    
  }
  
  pars <- recycle(from=head(breaks, -1), to=tail(breaks, -1), 
                  col=col, density=density, strict=FALSE)
  
  lst <- list()
  for(i in 1:attr(pars, "maxdim"))
    lst[[i]] <- .shade(pars$col[i], pars$from[i], pars$to[i], density=pars$density[i], n=n, ...)
  
  invisible(lst)
  
}


