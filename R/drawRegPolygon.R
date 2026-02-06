
#' Draw Regular Polygon(s) 
#' 
#' Draw a regular polygon with n corners. This is the workhorse function for
#' drawing regular polygons. Drawing a circle can be done by setting the
#' vertices to a value of say 100. 
#' 
#' All geometric arguments will be recycled.
#' 
#' @param x,y a vector (or scalar) of xy-coordinates of the center(s) of the
#' regular polygon(s). 
#' @param radius.x a scalar or a vector giving the semi-major axis of the
#' ellipse for the polygon(s). 
#' @param radius.y a scalar or a vector giving the semi-minor axis of the
#' ellipse for the polygon(s). Default is radius.x which will result in a
#' polygon with radius.x. 
#' @param rot angle of rotation in radians. 
#' @param nv number of vertices to draw the polygon(s). 
#' @param border color for borders. The default is \code{par("fg")}. Use
#' \code{border = NA} to omit borders. 
#' @param col color(s) to fill or shade the shape with. The default \code{NA}
#' (or also \code{NULL}) means do not fill (say draw transparent). 
#' @param lty line type for borders and shading; defaults to \code{"solid"}. 
#' @param lwd line width for borders and shading. 
#' @param plot logical. If \code{TRUE} the structure will be plotted. If
#' \code{FALSE} only the points are calculated and returned. Use this if you
#' want to combine several geometric structures to a polygon. 
#' @return The function invisibly returns a list of the calculated coordinates
#' for all shapes.
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{polygon}}, \code{\link{drawCircle}},
#' \code{\link{drawArc}} 
#' @keywords aplot
#' @examples
#' 
#' # Draw 4 triangles (nv = 3) with different rotation angles
#' plot(c(0,1),c(0,1), asp=1, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
#' drawRegPolygon(x = 0.5, y = 0.5, rot = (1:4)*pi/6, radius.x = 0.5, nv = 3,
#'   col = alpha("yellow",0.5))
#' 
#' 
#' # Draw several polygons
#' plot(c(0,1),c(0,1), asp=1, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
#' drawRegPolygon(x = 0.5, y = 0.5, radius.x=seq(50, 5, -10) * 1 /100,
#'   rot=0, nv = c(50, 10, 7, 4, 3), col=alpha("blue",seq(0.2,0.7,0.1)))
#' 
#' 
#' # Combine several polygons by sorting the coordinates
#' # Calculate the xy-points for two concentric pentagons
#' d.pts <- do.call("rbind", lapply(drawRegPolygon(radius.x=c(1,0.38), nv=5,
#'   rot=c(pi/2, pi/2+pi/5), plot=FALSE ), data.frame))
#' 
#' # prepare plot
#' plot(c(-1,1),c(-1,1), asp=1, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
#' 
#' # .. and draw the polygon with reordered points
#' polygon( d.pts[order(rep(1:6, times=2), rep(1:2, each=6)), c("x","y")], col="yellow")
#' 
#' 
#' 
#' # Move the center
#' plot(c(0,1),c(0,1), asp=1, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
#' theta <- seq(0, pi/6, length.out=5)
#' xy <- polToCart( exp(theta) /2, theta)
#' drawRegPolygon(x=xy$x, y=xy$y + 0.5, radius.x=seq(0.5, 0.1, -0.1),
#'   nv=4, rot=seq(0, pi/2, length.out=5), col=rainbow(5) )
#' 
#' 
#' # Plot a polygon with a "hole"
#' plot(c(-1,1),c(-1,1), asp=1, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
#' drawRegPolygon(nv = 4, rot=pi/4, col="red" )
#' text(x=0,y=0, "Polygon", cex=6, srt=45)
#' 
#' # Calculate circle and hexagon, but do not plot
#' pts <- drawRegPolygon(radius.x=c(0.7, 0.5), nv = c(100, 6), plot=FALSE )
#' 
#' # combine the 2 shapes and plot the new structure
#' polygon(x = unlist(lapply(pts, "[", "x")),
#'   y=unlist(lapply(pts, "[", "y")), col="green", border=FALSE)



#' @export
drawRegPolygon <- function( x = 0, y = x, radius.x = 1, radius.y = radius.x, rot = 0, 
                            nv = 3,
                            border = par("fg"), col = par("bg"), lty = par("lty"), 
                            lwd = par("lwd"), plot = TRUE ) {
  
  # The workhorse for the geom stuff
  
  # example:
  # plot(c(0,1),c(0,1), asp=1, type="n")
  # drawRegPolygon( x=0.5, y=0.5, radius.x=seq(0.5,0.1,-0.1), rot=0, nv=3:10, col=2)
  # drawRegPolygon( x=0.5+1:5*0.05, y=0.5, radius.x=seq(0.5,0.1,-0.1), rot=0, nv=100, col=1:5)
  
  # which geom parameter has the highest dimension
  lgp <- list(x=x, y=y, radius.x=radius.x, radius.y=radius.y, rot=rot, nv=nv)
  maxdim <- max(unlist(lapply(lgp, length)))
  # recycle all params to maxdim
  lgp <- lapply( lgp, rep, length.out=maxdim )
  
  # recycle shape properties
  if (length(col) < maxdim)    { col <- rep(col, length.out = maxdim) }
  if (length(border) < maxdim) { border <- rep(border, length.out = maxdim) }
  if (length(lwd) < maxdim)    { lwd <- rep(lwd, length.out = maxdim) }
  if (length(lty) < maxdim)    { lty <- rep(lty, length.out = maxdim) }
  
  lst <- list()   # prepare result
  for (i in 1:maxdim) {
    theta.inc <- 2 * pi / lgp$nv[i]
    theta <- seq(0, 2 * pi - theta.inc, by = theta.inc)
    ptx <- cos(theta) * lgp$radius.x[i] + lgp$x[i]
    pty <- sin(theta) * lgp$radius.y[i] + lgp$y[i]
    if(lgp$rot[i] > 0){
      # rotate the structure if the angle is > 0
      dx <- ptx - lgp$x[i]
      dy <- pty - lgp$y[i]
      ptx <- lgp$x[i] + cos(lgp$rot[i]) * dx - sin(lgp$rot[i]) * dy
      pty <- lgp$y[i] + sin(lgp$rot[i]) * dx + cos(lgp$rot[i]) * dy
    }
    if( plot )
      polygon(ptx, pty, border = border[i], col = col[i], lty = lty[i],
              lwd = lwd[i])
    lst[[i]] <- list(x = ptx, y = pty)
  }
  
  lst <- lapply(lst, xy.coords)
  if(length(lst)==1)
    lst <- lst[[1]]
  
  invisible(lst)
}


