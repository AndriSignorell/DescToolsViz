
#' Coordinates for "bottomright", etc. 
#' 
#' Return the xy.coordinates for the literal positions "bottomright", etc. as
#' used to place legends. 
#' 
#' The same logic as for the legend can be useful for placing texts, too. This
#' function returns the coordinates for the text, which can be used in the
#' specific text functions. 
#' 
#' @param x one out of \code{"bottomright"}, \code{"bottom"},
#' \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"},
#' \code{"topright"}, \code{"right"}, \code{"center"} 
#' @param region one out of \code{plot} or \code{figure}
#' @param cex the character extension for the text. 
#' @param linset line inset in lines of text.
#' @param \dots the dots are passed to the \code{strwidth()} and
#' \code{strheight()} functions in case there where more specific text formats.
#' 
#' @return nothing returned %% ~Describe the value returned %% If it is a LIST,
#' use %% \item{comp1 }{Description of 'comp1'} 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{text}}, \code{\link{boxedText}} 
#' @examples
#' 
#' plot(x = rnorm(10), type="n", xlab="", ylab="")
#' # note that plot.new() has to be called before we can grab the geometry
#' abcCoords("bottomleft")
#' 
#' lapply(c("bottomleft", "left"), abcCoords)
#' 
#' plot(x = rnorm(10), type="n", xlab="", ylab="")
#' text(x=(xy <- abcCoords("bottomleft", region = "plot"))$xy, 
#'      labels = "My Maybe Long Text", adj = xy$adj, xpd=NA)
#' 
#' text(x=(xy <- abcCoords("topleft", region = "figure"))$xy, 
#'      labels = "My Maybe Long Text", adj = xy$adj, xpd=NA)
#' 
#' plot(x = rnorm(10), type="n", xlab="", ylab="")
#' sapply(c("topleft", "top", "topright", "left", "center", 
#'          "right", "bottomleft", "bottom", "bottomright"), 
#'        function(x) 
#'          text(x=(xy <- abcCoords(x, region = "plot", linset=1))$xy, 
#'               labels = "MyMarginText", adj = xy$adj, xpd=NA)
#' )
#' 
#' 
#' plot(x = rnorm(100), type="n", xlab="", ylab="",
#'      panel.first={setBackCol(c("red", "lightyellow"))
#'              grid()})
#' xy <- abcCoords("topleft", region = "plot")
#' par(xpd=NA)
#' boxedText(x=xy$xy$x, y=xy$xy$y, xpad = 1, ypad = 1,
#'           labels = "My Maybe Long Text", adj = xy$adj, col=alpha("green", 0.8))
#' 


#' @export
abcCoords <- function(x="topleft", region="figure", 
                      cex=NULL, linset=0, ...) {
  
  region <- match.arg(region, c("figure", "plot", "device"))
  
  auto <- match.arg(x, c("bottomright", "bottom", "bottomleft",
                         "left", "topleft", "top", "topright", "right", "center"))
  
  
  # positioning code from legend()
  
  if(region %in% c("figure", "device")) {
    
    ds <- dev.size("in")
    # xy coordinates of device corners in user coordinates
    x <- grconvertX(c(0, ds[1]), from="in", to="user")
    y <- grconvertY(c(0, ds[2]), from="in", to="user")
    # fragment of the device we use to plot
    
    if(region == "figure") {
      fig <- par("fig")
      dx <- (x[2] - x[1])
      dy <- (y[2] - y[1])
      x <- x[1] + dx * fig[1:2]
      y <- y[1] + dy * fig[3:4]
    } 
  } else if(region == "plot"){
    
    usr <- par("usr")
    x <- usr[1:2]
    y <- usr[3:4]
    
  }
  
  
  linset <- rep(linset, length.out = 2)
  linsetx <- linset[1L] * strwidth("M", cex=1, units = "user", ...)
  x1 <- switch(auto, 
               bottomright = x[2] - linsetx, 
               topright = x[2] - linsetx,
               right = x[2] - linsetx, 
               bottomleft = x[1] + linsetx,
               left = x[1] + linsetx, 
               topleft = x[1] + linsetx, 
               bottom = (x[1] + x[2])/2,
               top = (x[1] + x[2])/2, 
               center = (x[1] + x[2])/2)
  
  linsety <- linset[2L] * strheight("M", cex=1, units = "user", ...)
  y1 <- switch(auto, 
               bottomright = y[1] + linsety, 
               bottom = y[1] + linsety, 
               bottomleft = y[1] + linsety, 
               topleft = y[2] - linsety, 
               top = y[2] - linsety, 
               topright = y[2] - linsety, 
               left = (y[1] + y[2])/2, 
               right = (y[1] + y[2])/2, 
               center = (y[1] + y[2])/2)
  
  adj <- switch(auto,
                topleft     =c(0,1),
                top         =c(0.5, 1),
                topright    =c(1,1),
                left        =c(0, 0.5),
                center      =c(0.5,0.5),
                right       =c(1, 0.5),
                bottomleft  =c(0,0),
                bottom      =c(0.5,0),
                bottomright =c(1,0))
  
  
  return(list(xy=xy.coords(x1, y1), adj=adj))
  
}

