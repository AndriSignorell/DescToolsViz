
#' Draw Elliptic Arc(s) 
#' 
#' Draw one or more elliptic (or circular) arcs from \code{theta.1} to
#' \code{theta.2} on an existing plot using classic graphics. 
#' 
#' All parameters are recycled if necessary. \cr Be sure to use an aspect ratio
#' of 1 as shown in the example to avoid distortion.
#' 
#' @param x,y a vector (or scalar) of xy-coordinates of the center(s) of the
#' arc(s). 
#' @param rx a scalar or a vector giving the semi-major axis of the ellipse for
#' the arc(s) 
#' @param ry a scalar or a vector giving the semi-minor axis of the ellipse for
#' the arc(s).  Default is radius.x which will result in a circle arc with
#' radius.x. 
#' @param theta.1 a scalar or a vector of starting angles in radians. 
#' @param theta.2 a scalar or a vector of ending angles in radians. 
#' @param nv number of vertices used to plot the arc. Scalar or vector. 
#' @param col color for the arc(s). Scalar or vector. 
#' @param lty line type used for drawing.
#' @param lwd line width used for drawing. 
#' @param plot logical. If \code{TRUE} the structure will be plotted. If
#' \code{FALSE} only the xy-points are calculated and returned.  Use this if
#' you want to combine several geometric structures to a single polygon.
#' 
#' @return \code{DrawArc} invisibly returns a list of the calculated
#' coordinates for all shapes.
#' 
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{drawCircle}}, \code{\link{polygon}} 
#' @keywords aplot
#' @examples
#' 
#' curve(sin(x), 0, pi, col="blue", asp=1)
#' drawArc(x = pi/2, y = 0, rx = 1, theta.1 = pi/4, theta.2 = 3*pi/4, col="red")
#' 

#' @export
drawArc <- function (x = 0, y = x, rx = 1, ry = rx, theta.1 = 0,
                     theta.2 = 2*pi, nv = 100, col = par("col"), lty = par("lty"),
                     lwd = par("lwd"), plot = TRUE) {
  
  # recycle all params to maxdim
  lgp <- recycle(x=x, y=y, rx = rx, ry = ry,
                 theta.1 = theta.1, theta.2 = theta.2, nv = nv,
                 col=col, lty=lty, lwd=lwd)
  
  lst <- list()
  for (i in 1L:attr(lgp, "maxdim")) {
    dthetha <- lgp$theta.2[i] - lgp$theta.1[i]
    
    theta <- seq(from = 0,
                 to = ifelse(dthetha < 0, dthetha + 2 * pi, dthetha),
                 length.out = lgp$nv[i]) + lgp$theta.1[i]
    
    ptx <- (cos(theta) * lgp$rx[i] + lgp$x[i])
    pty <- (sin(theta) * lgp$ry[i] + lgp$y[i])
    if (plot) {
      lines(ptx, pty, col = lgp$col[i], lty = lgp$lty[i], lwd = lgp$lwd[i])
    }
    lst[[i]] <- list(x = ptx, y = pty)
  }
  
  invisible(lst)
  
}


