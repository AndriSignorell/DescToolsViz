
#' Background of a Plot 
#' 
#' Paints the background of the plot, using either the figure region, the plot
#' region or both. It can sometimes be cumbersome to elaborate the coordinates
#' and base R does not provide a simple function for that. 
#' 
#' 
#' @param col the color of the background, if two colors are provided, the
#' first is used for the plot region and the second for the figure region. 
#' @param region either \code{"plot"} or \code{"figure"} 
#' @param border color for rectangle border(s). Default is \code{NA} for no
#' borders. 
#' 
#' @author Andri Signorell <andri@@signorell.net>
#' 
#' @seealso \code{\link{rect}} 
#' @examples
#' 
#' # use two different colors for the figure region and the plot region
#' plot(x = rnorm(100), col="blue", cex=1.2, pch=16,
#'      panel.first={setBackCol(c("red", "lightyellow"))
#'                   grid()})
#' 


#' @export
setBackCol <- function(col="grey", region=c("plot", "figure"), border=NA) {
  
  .setBackCol <- function(col="grey", region="plot", border=NA) {
    
    if(region=="plot")
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
           col = col, border=border)
    
    else if(region == "figure"){
      ds <- dev.size("in")
      # xy coordinates of device corners in user coordinates
      x <- grconvertX(c(0, ds[1]), from="in", to="user")
      y <- grconvertY(c(0, ds[2]), from="in", to="user")
      
      rect(x[1], y[2], x[2], y[1], 
           col = col, border=border, xpd=NA)
    }
  }
  
  if(length(col)==1){
    region <- match.arg(region)
    .setBackCol(col=col, region=region, border=border)
    
  } else {
    arg <- recycle(col=col, region=region, border=border)
    for(i in attr(arg, "maxdim"):1){
      .setBackCol(col=arg$col[i], region=arg$region[i], border=arg$border[i])
    }
    
  }
  
  
}

