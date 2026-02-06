
#' Draw a Bezier Curve 
#' 
#' Draw a Bezier curve. 
#' 
#' Bezier curves appear in such areas as mechanical computer aided design
#' (CAD).  They are named after P. Bezier, who used a closely related
#' representation in Renault's UNISURF CAD system in the early 1960s (similar,
#' unpublished, work was done by P. de Casteljau at Citroen in the late 1950s
#' and early 1960s). The 1970s and 1980s saw a flowering of interest in Bezier
#' curves, with many CAD systems using them, and many important developments in
#' their theory.  The usefulness of Bezier curves resides in their many
#' geometric and analytical properties.  There are elegant and efficient
#' algorithms for evaluation, differentiation, subdivision of the curves, and
#' conversion to other useful representations. (See: Farin, 1993)
#' 
#' @param x,y a vector of xy-coordinates to define the Bezier curve. Should at
#' least contain 3 points. 
#' @param nv number of vertices to draw the curve. 
#' @param col color(s) for the curve. Default is \code{par("fg")}.
#' @param lty line type for borders and shading; defaults to \code{"solid"}.
#' @param lwd line width for borders and shading.
#' @param plot logical. If \code{TRUE} the structure will be plotted. If
#' \code{FALSE} only the xy-points are calculated and returned.  Use this if
#' you want to combine several geometric structures to a single polygon.
#' 
#' @return \code{DrawBezier} invisibly returns a list of the calculated
#' coordinates for all shapes.
#' 
#' @author Frank E Harrell Jr <f.harrell@@vanderbilt.edu>
#' @seealso \code{\link{polygon}}, \code{\link{drawRegPolygon}},
#' \code{\link{drawCircle}}, \code{\link{drawArc}}
#' @references G. Farin (1993) \emph{Curves and surfaces for computer aided
#' geometric design. A practical guide}, Acad. Press
#' @keywords aplot
#' @examples
#' 
#' canvas(xlim=c(0,1))
#' grid()
#' drawBezier( x=c(0,0.5,1), y=c(0,0.5,0), col="blue", lwd=2)
#' drawBezier( x=c(0,0.5,1), y=c(0,1,0), col="red", lwd=2)
#' drawBezier( x=c(0,0.25,0.5,0.75,1), y=c(0,1,1,1,0), col="darkgreen", lwd=2)



#' @export
drawBezier <- function (x = 0, y = x, nv = 100,  col = par("col"), lty = par("lty")
                        , lwd = par("lwd"), plot = TRUE ) {
  
  if (missing(y)) {
    y <- x[[2]]
    x <- x[[1]]
  }
  n <- length(x)
  X <- Y <- single(nv)
  Z <- seq(0, 1, length = nv)
  X[1] <- x[1]
  X[nv] <- x[n]
  Y[1] <- y[1]
  Y[nv] <- y[n]
  for (i in 2:(nv - 1)) {
    z <- Z[i]
    xz <- yz <- 0
    const <- (1 - z)^(n - 1)
    for (j in 0:(n - 1)) {
      xz <- xz + const * x[j + 1]
      yz <- yz + const * y[j + 1]
      const <- const * (n - 1 - j)/(j + 1) * z/(1 - z)
      # debugging only:
      #            if (is.na(const)) print(c(i, j, z))
    }
    X[i] <- xz
    Y[i] <- yz
  }
  if(plot) lines(x = as.single(X), y = as.single(Y), col=col, lty=lty, lwd=lwd )
  invisible(list(x = as.single(X), y = as.single(Y)))
}



