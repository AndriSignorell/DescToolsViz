
#' Add an Alpha Channel To a Color
#' 
#' Add transparency to a color defined by its name or number. The function
#' first converts the color to RGB and then appends the alpha channel.
#' \code{fade()} combines \code{colToOpaque(SetAlpha(col))}. 
#' 
#' All arguments are recyled as necessary. 
#' 
#' @name alpha
#' @aliases alpha fade
#' @param col vector of two kind of R colors, i.e., either a color name (an
#' element of \code{colors()}) or an integer i meaning \code{palette()[i]}. 
#' @param alpha the alpha value to be added. This can be any value from 0
#' (fully transparent) to 1 (opaque). \code{NA} is interpreted so as to delete
#' a potential alpha channel. Default is 0.5. 
#' @param \dots the dots in \code{fade()} are passed on to \code{alpha}.
#' 
#' @return Vector with the same length as \code{col}, giving the rgb-values
#' extended by the alpha channel as hex-number (#rrggbbaa).
#' 
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{colToHex}}, \code{\link{col2rgb}},
#' \code{\link{adjustcolor}}, \code{\link{colToOpaque}}
#' @keywords color
#' @examples
#' 
#' alpha("yellow", 0.2)
#' alpha(2, 0.5)   # red
#' 
#' canvas(3)
#' drawCircle(x=c(-1,0,1), y=c(1,-1,1), r.out=2, col=alpha(2:4, 0.4))
#' 
#' x <- rnorm(15000)
#' par(mfrow=c(1,2))
#' plot(x, type="p", col="blue" )
#' plot(x, type="p", col=alpha("blue", .2), main="Better insight with alpha channel" )
#' 


#' @rdname alpha
#' @export
alpha <- function(col, alpha=0.5) {
  Vectorize(grDevices::adjustcolor)(col= col, alpha.f = alpha)
}


#' @rdname alpha
#' @export
fade <- function(col, ...){
  ColToOpaque(SetAlpha(col, ...))
}





