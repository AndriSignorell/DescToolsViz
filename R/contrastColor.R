
#' Choose Textcolor Depending on Background Color 
#' 
#' Text of a certain color when viewed against certain backgrounds can be hard
#' to see. \code{contrastColor} returns either black or white depending on
#' which has the better contrast. 
#' 
#' A simple heuristic in defining a text color for a given background color, is
#' to pick the one that is "farthest" away from "black" or "white". The way
#' Glynn chooses to do this is to compute the color intensity, defined as the
#' mean of the RGB triple, and pick "black" (intensity 0) for text color if the
#' background intensity is greater than 127, or "white" (intensity 255) when
#' the background intensity is less than or equal to 127. 
#' Sonego calculates 
#' \code{L <- c(0.2, 0.6, 0) \%*\% col2rgb(color)/255} and returns 
#' "black" if L >= 0.2 and "white" else. 
#' 
#' @param col vector of any of the three kind of R colors, i.e., either a color
#' name (an element of \code{colors()}), a hexadecimal string of the form
#' \code{"#rrggbb"} or \code{"#rrggbbaa"} (see \code{\link{rgb}}), or an
#' integer i meaning \verb{\code{palette()[i]}}.  Non-string values 
#' are coerced to integer. 
#' @param white the color for the dark backgrounds, default is \code{"white"}.
#' @param black the color for the bright backgrounds, default is \code{"black"}
#' @param method defines the algorithm to be used. Can be one out of
#' \code{"glynn"} or \code{"sonego"}. See details. 
#' 
#' @return a vector containing the contrast color (either black or white)
#' @author Andri Signorell <andri@@signorell.net> based on code of Earl F.
#' Glynn, Stowers Institute for Medical Research, 2004
#' 
#' @keywords color
#' 
#' @examples
#' # define some labels
#' lbl <- c("Butcher","Carpenter","Carter","Farmer","Hunter","Miller","Taylor")
#' 
#' # works fine for grays
#' plotArea( y=matrix(rep(1, times=3, each=8), ncol=8), x=1:3,
#'   col=gray(1:8 / 8), ylab="", xlab="", axes=FALSE )
#' text( x=2, y=1:8-0.5, lbl,
#'   col=contrastColor(gray(1:8 / 8)))
#' 
#' # and not so fine, but still ok, for colors
#' par(mfrow=c(1,2))
#' plotArea( y=matrix(rep(1, times=3, each=12), ncol=12), x=1:3,
#'   col=rainbow(12), ylab="", xlab="", axes=FALSE, main="method = Glynn" )
#' text( x=2, y=1:12-0.5, lbl,
#'   col=contrastColor(rainbow(12)))
#' 
#' plotArea( y=matrix(rep(1, times=3, each=12), ncol=12), x=1:3,
#'   col=rainbow(12), ylab="", xlab="", axes=FALSE, main="method = Sonego" )
#' text( x=2, y=1:12-0.5, lbl,
#'   col=contrastColor(rainbow(12), method="sonego"))
#' 


#' @export
contrastColor <- function(col, white="white", black="black", 
                          method=c("glynn","sonego")) {
  
  switch( match.arg( arg=method, choices=c("glynn","sonego") )
          , "glynn" = {
            # efg, Stowers Institute for Medical Research
            # efg's Research Notes:
            #   http://research.stowers-institute.org/efg/R/Color/Chart
            #
            # 6 July 2004.  Modified 23 May 2005.
            
            # For a given col, define a text col that will have good contrast.
            #   Examples:
            #     > GetTextContrastcol("white")
            #      "black"
            #     > GetTextContrastcol("black")
            #      "white"
            #     > GetTextContrastcol("red")
            #      "white"
            #     > GetTextContrastcol("yellow")
            #      "black"
            vx <- rep(white, length(col))
            vx[ apply(col2rgb(col), 2, mean) > 127 ] <- black
            
          }
          , "sonego" = {
            # another idea from Paolo Sonego in OneRTipaDay:
            L <- c(0.2, 0.6, 0) %*% col2rgb(col) / 255
            vx <- ifelse(L >= 0.2, black, white)
          }
  )
  
  return(vx)
  
}


