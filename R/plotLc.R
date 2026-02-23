#' Plot and Add Lines for Lorenz Curve Objects
#'
#' S3 methods for visualising objects of class \code{"Lc"}.
#' The \code{plot} method draws a Lorenz curve, while
#' \code{lines} adds a Lorenz curve (optionally with confidence band)
#' to an existing plot.
#'
#' @param x Object of class \code{"Lc"} as returned by \code{\link[DescToolsX]{lc}}.
#' @param general Logical. If \code{TRUE}, the generalised Lorenz curve
#'   is plotted.
#' @param lwd Line width.
#' @param type Plot type (see \code{\link{plot}}).
#' @param xlab,ylab Axis labels.
#' @param main Plot title.
#' @param las Axis label style.
#' @param pch Optional plotting character for points.
#' @param conf.level Confidence level for the confidence band.
#'   If \code{NA}, no band is drawn.
#' @param args.cband List of graphical arguments passed to
#'   \code{\link{drawBand}} (e.g. \code{col}, \code{border}).
#' @param ... Additional graphical parameters passed to
#'   \code{\link{plot}} or \code{\link{lines}}.
#'
#' @return Invisibly returns the input object.
#'
#' @family Inequality
#' @concept Inequality
#' @concept Lorenz Curve
#'


#' @export
#' @method plot Lc
plot.Lc <- function(x, general=FALSE, lwd=2, type="l", xlab="p", ylab="L(p)",
                    main="Lorenz curve", las=1, pch=NA, ...)  {
  if(!general)
    L <- x$L
  else
    L <- x$L.general
  plot(x$p, L, type=type, main=main, lwd=lwd, xlab=xlab, ylab=ylab, xaxs="i",
       yaxs="i", las=las, ...)
  
  abline(0, max(L))
  
  if(!is.na(pch)){
    opar <- par(xpd=TRUE)
    on.exit(par(opar))
    points(x$p, L, pch=pch, ...)
  }
  
}


#' @param args.cband list of arguments for the confidence band, such as color
#' or border (see \code{\link{drawBand}}). 
 
#' @rdname plot.Lc
#' @method lines Lc
#' @export
lines.Lc <- function(x, general=FALSE, lwd=2, conf.level = NA, 
                     args.cband = NULL, ...) {
  
  if(!general)
    L <- x$L
  else
    L <- x$L.general
  
  
  if (!(is.na(conf.level) || identical(args.cband, NA)) ) {
    
    args.cband1 <- .mergeArgs(defaults = list(
      col    = alpha(.getOption("palette")[1], 0.12), 
      border = NA),
      args.cband)
    
    ci <- predict(object=x, conf.level=conf.level, general=general)

    do.call("drawBand", c(args.cband1, list(x=c(ci$p, rev(ci$p))),
                          list(y=c(ci$lci, rev(ci$uci)))))
    
  }
  
  
  lines(x$p, L, lwd=lwd, ...)
  
}


#' @export
plot.Lclist <- function(x, col=1, lwd=2, lty=1, main = "Lorenz curve",
                        xlab="p", ylab="L(p)", ...){
  
  # Recycle arguments
  lgp <- recycle(x=seq_along(x), col=col, lwd=lwd, lty=lty)
  
  plot(x[[1]], col=lgp$col[1], lwd=lgp$lwd[1], lty=lgp$lty[1], main=main, xlab=xlab, ylab=ylab, ...)
  for(i in 2L:length(x)){
    lines(x[[i]], col=lgp$col[i], lwd=lgp$lwd[i], lty=lgp$lty[i])
  }
}

