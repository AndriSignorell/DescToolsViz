
#' Plot a Correlation Matrix
#' 
#' This function produces a graphical display of a correlation matrix. The
#' cells of the matrix can be shaded or colored to show the correlation value.
#' 
#' 
#' @param x x is a correlation matrix to be visualized. 
#' @param cols the colors for shading the matrix. Uses the package's option
#' \code{"col1"} and \code{"col2"} as default. 
#' @param breaks a set of breakpoints for the colours: must give one more
#' breakpoint than colour. These are passed to \code{image()} function.  If
#' breaks is specified then the algorithm used follows \code{\link{cut}}, so
#' intervals are closed on the right and open on the left except for the lowest
#' interval. 
#' @param border color for borders. The default is \code{grey}. Set this
#' argument to \code{NA} if borders should be omitted.
#' @param lwd line width for borders. Default is 1. 
#' @param args.colorLegend list of arguments for the \code{\link{colorLegend}}.
#' Use \code{NA} if no color legend should be painted. 
#' @param xaxt parameter to define, whether to draw an x-axis, defaults to
#' \code{"n"}. 
#' @param yaxt parameter to define, whether to draw an y-axis, defaults to
#' \code{"n"}. 
#' @param cex.axis character extension for the axis labels. 
#' @param las the style of axis labels. 
#' @param mar sets the margins, defaults to mar = c(3, 8, 8, 8) as we need a
#' bit more room on the right. 
#' @param mincor numeric value between 0 and 1, defining the smallest
#' correlation that is to be displayed. If this is >0 then all correlations
#' with a lower value are suppressed.
#' @param main character, the main title.
#' @param clust logical. If set to \code{TRUE}, the correlations will be
#' clustered in order to aggregate similar values.
#' @param \dots the dots are passed to the function \code{\link{image}}, which
#' produces the plot. 
#' @return no values returned. 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{image}}, \code{\link{colorLegend}},
#' \code{\link[corrgram]{corrgram}()}, \code{\link{plotWeb}()} 
#' @keywords hplot multivariate
#' @examples
#' 
#' m <- cor(swiss)
#' 
#' plotCorr(m, cols=colorRampPalette(c("red", "black", "green"), space = "rgb")(20))
#' plotCorr(m, cols=colorRampPalette(c("red", "black", "green"), space = "rgb")(20),
#'          args.colorLegend=NA)
#' 
#' m <- cor(mtcars)
#' plotCorr(m, col=Pal("RedWhiteBlue1", 100), border="grey",
#'          args.colorLegend=list(labels=format(seq(-1,1,.25), digits=2), frame="grey"))
#' 
#' # display only correlation with a value > 0.7
#' plotCorr(m, mincor = 0.7)
#' x <- matrix(rep(1:ncol(m),each=ncol(m)), ncol=ncol(m))
#' y <- matrix(rep(ncol(m):1,ncol(m)), ncol=ncol(m))
#' txt <- format(m, digits=3)
#' idx <- upper.tri(matrix(x, ncol=ncol(m)), diag=FALSE)
#' 
#' # place the text on the upper triagonal matrix
#' text(x=x[idx], y=y[idx], label=txt[idx], cex=0.8, xpd=TRUE)
#' 
#' # put similiar correlations together
#' plotCorr(m, clust=TRUE)
#' 
#' # same as
#' idx <- order.dendrogram(as.dendrogram(
#'           hclust(dist(m), method = "mcquitty")
#'        ))
#' plotCorr(m[idx, idx])
#' 
#' # plot only upper triangular matrix and move legend to bottom
#' m <- cor(mtcars)
#' m[lower.tri(m, diag=TRUE)] <- NA
#' 
#' # get the p-values
#' p <- outer(
#'   (vars <- colnames(mtcars)), vars,
#'   Vectorize(function(v1, v2)
#'     cor.test(mtcars[[v1]], mtcars[[v2]], method = "pearson")$p.value
#'   )
#' )
#' dimnames(p) <- list(vars, vars)
#' m[p > 0.05] <- NA
#' 
#' plotCorr(m, mar=c(8,8,8,8), yaxt="n",
#'          args.colorLegend = list(x="bottom", inset=-.15, horiz=TRUE, 
#'                                  height=abs(lineToUser(line = 2.5, side = 1)), 
#'                                  width=ncol(m)))
#' mtext(text = rev(rownames(m)), side = 4, at=1:ncol(m), las=1, line = -5, cex=0.8)
#' 



#' @export
plotCorr <- function(x, cols = colorRampPalette(c(Pal()[2], "white", Pal()[1]), space = "rgb")(20)
                     , breaks = seq(-1, 1, length = length(cols)+1), border="grey", lwd=1
                     , args.colorLegend = NULL, xaxt = par("xaxt"), yaxt = par("yaxt"), cex.axis = 0.8, las = 2
                     , mar = c(3,8,8,8), mincor=0, main="", clust=FALSE, ...){
  
  # example:
  # m <- cor(d.pizza[,WhichNumerics(d.pizza)][,1:5], use="pairwise.complete.obs")
  # plotCorr(m)
  # plotCorr(m, args.colorLegend="n", las=1)
  # plotCorr(m, cols=colorRampPalette(c("red", "white", "blue"), space = "rgb")(4), args.colorLegend=list(xlab=sprintf("%.1f", seq(1,-1, length=5))) )
  # plotCorr(m, cols=colorRampPalette(c("red", "black", "green"), space = "rgb")(10))
  
  # plotCorr(round(CramerV(d.pizza[,c("driver","operator","city", "quality")]),3))
  
  pars <- par(mar=mar); on.exit(par(pars))
  
  # matrix should be transposed to allow upper.tri with the corresponding representation
  x <- t(x)
  
  if(clust==TRUE) {
    # cluster correlations in order to put similar values together
    idx <- order.dendrogram(as.dendrogram(
      hclust(dist(x), method = "mcquitty")
    ))
    
    x <- x[idx, idx]
  }
  
  # if mincor is set delete all correlations with abs. val. < mincor
  if(mincor!=0)
    x[abs(x) < abs(mincor)] <- NA
  
  x <- x[,ncol(x):1]
  image(x=1:nrow(x), y=1:ncol(x), xaxt="n", yaxt="n", z=x, frame.plot=FALSE, xlab="", ylab=""
        , col=cols, breaks=breaks, ... )
  if(xaxt!="n") axis(side=3, at=1:nrow(x), labels=rownames(x), cex.axis=cex.axis, las=las, lwd=-1)
  if(yaxt!="n") axis(side=2, at=1:ncol(x), labels=colnames(x), cex.axis=cex.axis, las=las, lwd=-1)
  
  if((is.list(args.colorLegend) || is.null(args.colorLegend))){
    
    # bugfix dmurdoch 7.2.2022
    digits <- round(1 - log10(diff(range(breaks))))
    args.colorLegend1 <- list( labels=sprintf("%.*f", digits,
                                              breaks[seq(1,length(breaks), by = 2)])
                               # args.colorLegend1 <- list( labels=sprintf("%.1f", seq(-1,1, length=length(cols)/2+1))
                               , x=nrow(x)+0.5 + nrow(x)/20, y=ncol(x)+0.5
                               , width=nrow(x)/20, height=ncol(x), cols=cols, cex=0.8 )
    if ( !is.null(args.colorLegend) ) { args.colorLegend1[names(args.colorLegend)] <- args.colorLegend }
    
    do.call("colorLegend", args.colorLegend1)
  }
  
  if(!is.na(border)) {
    usr <- par("usr")
    rect(xleft=0.5, xright=nrow(x)+0.5, ybottom=0.5, ytop=nrow(x)+0.5,
         lwd=lwd, border=border)
    usr <- par("usr")
    clip(0.5, nrow(x)+0.5, 0.5, nrow(x)+0.5)
    abline(h=seq(-2, nrow(x)+1,1)-0.5, v=seq(1,nrow(x)+1,1)-0.5, col=border,lwd=lwd)
    do.call("clip", as.list(usr))
  }
  
  if(!is.null(.getOption("stamp")))
    stamp()
  
  if(main!="") title(main=main)
  
}

###
