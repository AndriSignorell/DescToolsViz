
#' Get Color on a Defined Color Range
#' 
#' Find a color on a defined color range depending on the value of x. This is
#' helpful for colorcoding numeric values.
#' 
#' For the selection of colors the option \code{rightmost.closed} in the used
#' function \code{\link{findInterval}} is set to TRUE. This will ensure that
#' all values on the right edge of the range are assigned a color. How values
#' outside the boundaries of min.x and max.x should be handled can be
#' controlled by \code{all.inside}. Set this value to TRUE, if those values
#' should get the colors at the edges or set it to FALSE, if they should remain
#' white (which is the default).
#' 
#' Note that \code{\link{findInterval}} closes the intervals on the left side,
#' e.g. [0, 1). This option can't be changed. Consequently will x-values lying
#' on the edge of two colors get the color of the bigger one.
#' 
#' @param x numeric.
#' @param cols a vector of colors.
#' @param min.x the x-value to be used for the left edge of the first color. If
#' left to the default \code{NULL} \code{min(pretty(x))} will be used.
#' @param max.x the x-value to be used for the right edge of the last color. If
#' left to the default \code{NULL} \code{max(pretty(x))} will be used.
#' @param all.inside logical; if true, the returned indices are coerced into
#' \code{1, ..., N-1}, i.e., \code{0} is mapped to \code{1} and \code{N} to
#' \code{N-1}.
#' 
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{findInterval}}
#' @keywords color dplot
#' @examples
#' 
#' 
#' canvas(7, main="Use of function findColor()")
#' 
#' # get some data
#' x <- c(23, 56, 96)
#' # get a color range from blue via white to red
#' cols <- colorRampPalette(c("blue","white","red"))(100)
#' colorLegend(x="bottomleft", cols=cols, labels=seq(0, 100, 10), cex=0.8)
#' 
#' # and now the color coding of x:
#' (xcols <- findColor(x, cols, min.x=0, max.x=100))
#' 
#' # this should be the same as
#' cols[x+1]
#' 
#' # how does it look like?
#' y0 <- c(-5, -2, 1)
#' text(x=1, y=max(y0)+2, labels="Color coding of x:")
#' text(x=1.5, y=y0, labels=x)
#' drawRegPolygon(x=3, y=y0, nv=4, rot=pi/4, col=xcols)
#' text(x=6, y=y0, labels=xcols)
#' 
#' # how does the function select colors?
#' canvas(xlim = c(0,1), ylim = c(0,1))
#' cols <- c(red="red", yellow="yellow", green="green", blue="blue")
#' colorLegend(x=0, y=1, width=1, cols=rev(cols), horiz = TRUE,
#'             labels=format(seq(0, 1, .25), digits=2, nsmall=2), 
#'             frame="grey", cex=0.8 )
#' x <- c(-0.2, 0, 0.15, 0.55, .75, 1, 1.3)
#' arrows(x0 = x, y0 = 0.6, y1 = 0.8, angle = 15, length = .2)
#' text(x=x, y = 0.5, labels = x, adj = c(0.5,0.5))
#' text(x=x, y = 0.4, labels = names(findColor(x, cols=cols,
#'    min.x = 0, max.x = 1, all.inside = TRUE)), adj = c(0.5,0.5))
#' text(x=x, y = 0.3, labels = names(findColor(x, cols=cols,
#'    min.x = 0, max.x = 1, all.inside = FALSE)), adj = c(0.5,0.5))


#' @export
findColor <- function(x, cols=rev(heat.colors(100)), min.x=NULL, max.x=NULL,
                      all.inside = FALSE){
  
  if(is.null(min.x)) min.x <- min(pretty(x))
  if(is.null(max.x)) max.x <- max(pretty(x))
  
  # Korrektur von min und max, wenn nicht standardmaessig
  colrange <- range(c(min.x, max.x))
  
  # Berechnung des entsprechenden Farb-Index
  col.idx <- findInterval(x, seq(colrange[1], colrange[2], length = length(cols) + 1)
                          , rightmost.closed=TRUE, all.inside=all.inside)
  col.idx[col.idx==0] <- NA  # den Index 0 gibt es nicht im Farbenvektor
  cols[col.idx]
  
  # alt:
  # cols[ findInterval( x, seq(colrange[1], colrange[2], length=length(cols)+1 ) ) ]
}
