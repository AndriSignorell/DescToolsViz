
#' Convert Line Coordinates To User Coordinates 
#' 
#' Functions like \code{mtext} or \code{axis} use the \code{line} argument to
#' set the distance from plot. Sometimes it's useful to have the distance in
#' user coordinates. \code{LineToUser()} does this nontrivial conversion. 
#' 
#' For the \code{LineToUser} function to work, there must be an open plot.
#' 
#' @param line the number of lines 
#' @param side the side of the plot 
#' @return the user coordinates for the given lines 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{mtext}} 
#' @keywords aplot
#' @examples
#' 
#' plot(1:10)
#' lineToUser(line=2, side=4)
#' 

 
#' @export 
lineToUser <- function(line, side) {
  
  # http://stackoverflow.com/questions/29125019/get-margin-line-locations-mgp-in-user-coordinates
  # jbaums

  lh <- par('cin')[2] * par('cex') * par('lheight')
  
  x_off <- diff(grconvertX(0:1, 'inches', 'user'))
  y_off <- diff(grconvertY(0:1, 'inches', 'user'))
  
  switch(side,
         `1` = par('usr')[3] - line * y_off * lh,
         `2` = par('usr')[1] - line * x_off * lh,
         `3` = par('usr')[4] + line * y_off * lh,
         `4` = par('usr')[2] + line * x_off * lh,
         stop("side must be 1, 2, 3, or 4", call.=FALSE))

}



