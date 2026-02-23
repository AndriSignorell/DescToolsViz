
#' Plot Probability Distribution 
#' 
#' Produce a plot from a probability distribution with shaded areas. This is
#' often needed in theory texts for classes in statistics.  
#' 
#' The function sets up a two-step plot procedure based on curve() and shade()
#' with additional labelling for convenience. 
#' 
#' @param breaks a numeric vector containing the breaks of different areas. The
#' start and end must not be infinity. 
#' @param FUN the (typically) distribution function 
#' @param blab text for labelling the breaks 
#' @param main main title for the plot 
#' @param xlim the x-limits for the plot 
#' @param col the color for the shaded areas 
#' @param density the density for the shaded areas 
#' @param alab the labels for areas
#' @param alab_x the x-coord for the area labels 
#' @param alab_y the y-coord for the area labels, if left to default they will
#' be placed in the middle of the plot 
#' @param ylab the label for they y-axis
#' @param \dots further parameters passed to internally used function
#' \code{\link{curve}()}
#' 
#' @return nothing returned 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{shade}}, \code{\link{curve}}, \code{\link{polygon}} 
#' 
#' @examples
#' 
#' # plot t-distribution
#' plotProbDist(breaks=c(-6, -2.3, 1.5, 6), 
#'              function(x) dt(x, df=8), 
#'              blab=c("A","B"), xlim=c(-4,4), alab=NA,
#'              main="t-Distribution (df=8)",
#'              col=c("deeppink4", "skyblue3", "darkorange2"), 
#'              density=c(20, 7))
#' 
#' # Normal
#' plotProbDist(breaks=c(-10, -1, 12), 
#'              function(x) dnorm(x, mean=2, sd=2), 
#'              blab="A", xlim=c(-7,10),
#'              main="Normal-Distribution N(2,2)",
#'              col=c("deeppink4", "skyblue3""), 
#'              density=c(20, 7))
#' 
#' # same for Chi-square
#' plotProbDist(breaks=c(0, 15, 35), 
#'              function(x) dchisq(x, df=8), 
#'              blab="B", xlim=c(0, 30),
#'              main=expression(paste(chi^2-Distribution, " (df=8)")),
#'              col=c("deeppink4", "skyblue3"), density=c(0, 20))
#' 
#' 



#' @export
plotProbDist <- function(breaks, FUN, blab=NULL, main="", xlim=NULL, 
                         col=NULL, density=7, 
                         alab = LETTERS[1:(length(breaks)-1)], 
                         alab_x=NULL, alab_y = NULL, ylab="density", ...){
  
  fct <- FUN
  FUN <- "fct"
  FUN <- eval(parse(text = FUN))
  
  
  if(is.null(col))
    col <- Pal("Helsana")[1:length(breaks)]
  
  curve(FUN, xlim=xlim,
        main=main,
        type="n", las=1, ylab=ylab, ...)
  
  shade(FUN, breaks=breaks,
        col=col, density=density)
  
  if(is.null(alab_x))
    alab_x <- moveAvg(c(xlim[1], head(breaks, -1)[-1], xlim[2]), order=2, align="left")
  
  if(is.null(alab_y))
    alab_y <- abcCoords("left")$xy$y
  
  if(!identical(alab, NA))
    boxedText(labels = alab,
              x=alab_x, y=alab_y, cex=2, border=NA)
  
  if(!is.null(blab)){
    mtext(blab, side=1, line=2.5, at=head(breaks, -1)[-1], font=2, cex=1.4)
  }
  
}

