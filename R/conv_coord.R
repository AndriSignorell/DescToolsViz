
#' Coordinate Transformations Cartesian/Polar/Spherical
#' 
#' Transform cartesian into polar coordinates, resp. to spherical coordinates
#' and vice versa. 
#' 
#' Angles are in radians, not degrees (i.e., a right angle is pi/2). Use
#' \code{\link[DescToolsX]{degToRad}} to convert, if you don't wanna do it by yourself.\cr
#' All parameters are recycled if necessary. 
#' 
#' @name conv_coord
#' @aliases cartToPol polToCart cartToSph sphToCart
#' @param x,y,z vectors with the xy-coordianates to be transformed. 
#' @param r a vector with the radius of the points. 
#' @param theta a vector with the angle(s) of the points. 
#' @param phi a vector with the angle(s) of the points. 
#' @param up logical. If set to \code{TRUE} (default) theta is measured from
#' x-y plane, else theta is measured from the z-axis. 
#' 
#' @return \code{polToCart()} returns a list of x and y coordinates of the points.\cr
#' \code{cartToPol()} returns a list of r for the radius and theta for the angles of the
#' given points.
#' 
#' @author Andri Signorell <andri@@signorell.net>, Christian W. Hoffmann
#' <christian@@echoffmann.ch> 
#' @keywords arith dplot
#' @examples
#' 
#' cartToPol(x=1, y=1)
#' cartToPol(x=c(1,2,3), y=c(1,1,1))
#' cartToPol(x=c(1,2,3), y=1)
#' 
#' 
#' polToCart(r=1, theta=pi/2)
#' polToCart(r=c(1,2,3), theta=pi/2)
#' 
#' polToCart(r=c(1,2,3), theta=pi/2)
#' 
#' cartToSph(x=1, y=2, z=3)   # r=3.741657, theta=0.930274, phi=1.107149
#' 

#' @export
#' @rdname conv_coord
polToCart <- function(r, theta) list(x=r*cos(theta), y=r*sin(theta))

#' @export
#' @rdname conv_coord
cartToPol <- function(x, y) {
  theta <- atan(y/x)
  theta[x<0] <- theta[x<0] + pi    # atan can't find the correct square (quadrant)
  list(r = sqrt(x^2 + y^2), theta=theta)
}


#' @export
#' @rdname conv_coord
cartToSph <- function (x, y, z, up = TRUE ) {
  
  vphi <- cartToPol(x, y)          # x, y -> c( w, phi )
  R <- if (up) {
    cartToPol(vphi$r, z)          # ( w, z,  -> r, theta )
  } else {
    cartToPol(z, vphi$r)          # ( z, w,  -> r, theta )
  }
  res <- c(R[1], R[2], vphi[2])
  names(res) <- c("r", "theta", "phi")
  
  return (res)
}


#' @export
#' @rdname conv_coord
sphToCart <- function (r, theta, phi, up = TRUE) {
  
  if (up) theta <- pi/2 - theta
  
  vz <- polToCart(r, theta)
  xy <- polToCart(vz$y, phi)
  
  res <- list(x=xy$x, y=xy$x, z=vz$x)
  
  return (res)
}


