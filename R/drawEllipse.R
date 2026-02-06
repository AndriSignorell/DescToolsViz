

#' Draw an Ellipse 
#' 
#' Draw one or several ellipses on an existing plot. 
#' 
#' Use \code{\link[DescToolsX]{degToRad}} if you want to define rotation angle in degrees.
#' 
#' @param x,y the x and y co-ordinates for the centre(s) of the ellipse(s). 
#' @param radius.x a scalar or a vector giving the semi-major axis of the
#' ellipse. 
#' @param radius.y a scalar or a vector giving the semi-minor axis of the
#' ellipse. 
#' @param rot angle of rotation in radians. 
#' @param nv number of vertices to draw the ellipses. 
#' @param border color for borders. The default is \code{par("fg")}. Use
#' \code{border = NA} to omit borders.
#' @param col color(s) to fill or shade the annulus sector with. The default
#' \code{NA} (or also \code{NULL}) means do not fill (say draw transparent). 
#' @param lty line type for borders and shading; defaults to \code{"solid"}. 
#' @param lwd line width for borders and shading. 
#' @param plot logical. If \code{TRUE} the structure will be plotted. If
#' \code{FALSE} only the points are calculated and returned. Use this if you
#' want to combine several geometric structures to a single polygon. 
#' 
#' @return The function invisibly returns a list of the calculated coordinates
#' for all shapes. 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{polygon}}, \code{\link{drawRegPolygon}},
#' \code{\link{drawCircle}}, \code{\link{drawArc}} 
#' @keywords aplot
#' @examples
#' 
#' par(mfrow=c(1,2))
#' 
#' canvas()
#' drawEllipse(rot = c(1:3) * pi/3, col=alpha(c("blue","red","green"), 0.5) )
#' 
#' 
#' plot(cars)
#' m <- var(cars)
#' eig <- eigen(m)
#' eig.val <- sqrt(eig$values)
#' eig.vec <- eig$vectors
#' 
#' drawEllipse(x=mean(cars$speed), y=mean(cars$dist), 
#'             radius.x=eig.val[1] , radius.y=eig.val[2], 
#'             rot=acos(eig.vec[1,1]), border="blue", lwd=3)


#' @export
drawEllipse <- function( x = 0, y = x, radius.x = 1, radius.y = 0.5, rot = 0, 
                         nv = 100, border = par("fg"), col = par("bg")
                         , lty = par("lty"), lwd = par("lwd"), plot = TRUE ) {
  invisible( drawRegPolygon(  x = x, y = y, radius.x = radius.x, radius.y = radius.y, 
                              nv = nv, rot = rot
                              , border = border, col = col, lty = lty, lwd = lwd, 
                              plot = plot ) )
}

