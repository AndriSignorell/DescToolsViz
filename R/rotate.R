
#' Rotate a Geometric Structure 
#' 
#' Rotate a geometric structure by an angle theta around a centerpoint xy. 
#' 
#' 
#' @param x,y vectors containing the coordinates of the vertices of the polygon
#' , which has to be rotated.  The coordinates can be passed in a plotting
#' structure (a list with x and y components), a two-column matrix, .... See
#' \code{\link{xy.coords}}. 
#' @param mx,my xy-coordinates of the center of the rotation. If left to NULL,
#' the centroid of the structure will be used. 
#' @param theta angle of the rotation 
#' @param asp the aspect ratio for the rotation. Helpful for rotate structures
#' along an ellipse.
#' 
#' @return The function invisibly returns a list of the coordinates for the
#' rotated shape(s). 
#' 
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{polygon}}, \code{\link{drawRegPolygon}},
#' \code{\link{drawEllipse}}, \code{\link{drawArc}} 
#' @keywords aplot
#' @examples
#' 
#' # let's have a triangle
#' canvas(main="Rotation")
#' x <- drawRegPolygon(nv=3)[[1]]
#' 
#' # and rotate
#' sapply( (0:3) * pi/6, function(theta) {
#'   xy <- rotate( x=x, theta=theta )
#'   polygon(xy, col=alpha("blue", 0.2))
#' } )
#' 
#' abline(v=0, h=0)


#' @export
rotate <- function( x, y=NULL, mx = NULL, my = NULL, theta=pi/3, asp=1 ) {
  
  # # which geom parameter has the highest dimension
  # lgp <- list(x=x, y=y)
  # maxdim <- max(unlist(lapply(lgp, length)))
  # # recycle all params to maxdim
  # lgp <- lapply( lgp, rep, length.out=maxdim )
  
  # polygon doesn't do that either!!
  
  xy <- xy.coords(x, y)
  
  if(is.null(mx))
    mx <- mean(xy$x)
  
  if(is.null(my))
    my <- mean(xy$y)
  
  # rotate the structure
  dx <- xy$x - mx
  dy <- xy$y - my
  ptx <- mx + cos(theta) * dx - sin(theta) * dy / asp
  pty <- my + sin(theta) * dx * asp + cos(theta) * dy
  
  return(xy.coords(x=ptx, y=pty))
  
}
