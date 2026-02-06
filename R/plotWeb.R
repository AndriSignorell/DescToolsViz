
#' Plot a Web of Connected Points 
#' 
#' This plot can be used to graphically display a correlation matrix by using
#' the linewidth between the nodes in proportion to the correlation of two
#' variables. It will place the elements homogenously around a circle and draw
#' connecting lines between the points.
#' 
#' The function uses the lower triangular matrix of \code{m}, so this is the
#' order colors, linewidth etc. must be given, when the defaults are to be
#' overrun.
#' 
#' @param m a symmetric matrix of numeric values 
#' @param col the color for the connecting lines 
#' @param lty the line type for the connecting lines, the default will be
#' \code{par("lty")}. 
#' @param lwd the line widths for the connecting lines. If left to \code{NULL}
#' it will be linearly scaled between the minimum and maximum value of
#' \code{m}. 
#' @param args.legend list of additional arguments to be passed to the
#' \code{legend} function.  Use \code{args.legend = NA} if no legend should be
#' added. 
#' @param pch the plotting symbols appearing in the plot, as a non-negative
#' numeric vector (see \code{\link{points}}, but unlike there negative values
#' are omitted) or a vector of 1-character strings, or one multi-character
#' string.
#' @param pt.cex expansion factor(s) for the points.
#' @param pt.col the foreground color for the points, corresponding to its
#' argument \code{col}.
#' @param pt.bg the background color for the points, corresponding to its
#' argument \code{bg}.
#' @param las alignment of the labels, 1 means horizontal, 2 radial and 3
#' vertical. 
#' @param adj adjustments for the labels. (Left: 0, Right: 1, Mid: 0.5) 
#' @param dist gives the distance of the labels from the outer circle. Default
#' is 2.
#' @param cex.lab the character extension for the labels. 
#' @param \dots dots are passed to plot. 
#' @return A list of x and y coordinates, giving the coordinates of all the
#' points drawn, useful for adding other elements to the plot.
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{plotCorr}}
#' @keywords hplot multivariate
#' @examples
#' 
#' m <- cor(swiss)
#' plotWeb(m=m, main="Swiss correlation")
#' 
#' 
#' # let's describe only the significant corrs and start with a dataset
#' # get the p-values
#' p <- outer(
#'   (vars <- colnames(mtcars)), vars,
#'   Vectorize(function(v1, v2)
#'     cor.test(mtcars[[v1]], mtcars[[v2]], method = "pearson")$p.value
#'   )
#' )
#' dimnames(p) <- list(vars, vars)
#' 
#' # ok, got all the p-values, now replace > 0.05 with NAs
#' m[p > 0.05] <- NA
#' 
#' # How does that look like now?
#' # try also: fm(m, na.form = ". ", ldigits=0, digits=3, align = "right")
#' 
#' # plotWeb(m, las=2, cex=1.2)
#' 
#' # define line widths
#' # plotWeb(m, lwd=abs(m[lower.tri(m)] * 10))
#' 


#' @export
plotWeb <- function(m, col=NULL, lty=NULL, 
                    lwd = NULL, args.legend=NULL, pch=21, pt.cex=2,
                    pt.col="black", pt.bg="darkgrey", cex.lab = 1.0,
                    las = 1, adj = NULL, dist = 0.5, ... ){
  
  # following an idee from library(LIM)
  # example(plotWeb)
  
  
  .withGraphicsState({
    
    # qqplot for an optional distribution
    
    # example:
    # y <- rexp(100, 1/10)
    # plotQQ(y, function(p) qexp(p, rate=1/10))
    
    
    # resolve potential pars given in the dots...

    col <- col %||% .getOption("palette", default = c("firebrick", "steelblue"))[1:2]
    
    .applyParFromDots(...)

    canvas(4, ...)
    angles <- seq(0, 2*pi, length=nrow(m)+1)[-1]
    xy <- polToCart(r=3, theta=angles)
    xylab <- polToCart(r=3 + dist, theta=angles)
    
    labels <- colnames(m)
    
    if(las == 2){
      if(is.null(adj)) adj <- (angles >= pi/2 & angles <= 3*pi/2)*1
      adj <- rep(adj, length_out=length(labels))
      sapply(seq_along(labels),
             function(i) text(xylab$x[i], xylab$y[i], labels=labels[i], cex=cex.lab,
                              srt=radToDeg(atan(xy$y[i]/xy$x[i])), adj=adj[i]))
    } else {
      if(is.null(adj)){
        if(las==1)
          adj <- (angles >= pi/2 & angles <= 3*pi/2)*1
        if(las==3)
          adj <- (angles >= 3*pi/4 & angles <= 7*pi/4)*1
      }
      adj <- rep(adj, length_out=length(labels))
      sapply(seq_along(labels),
             function(i) text(xylab$x[i], xylab$y[i], labels=labels[i], cex=cex.lab,
                              srt=ifelse(las==3, 90, 0), adj=adj[i]))
      
    }
    
    # d.m <- data.frame( from=rep(colnames(m), nrow(m)), to=rep(colnames(m), each=nrow(m))
    #   , d=as.vector(m)
    #   , from.x=rep(xy$x, nrow(m)), from.y=rep(xy$y, nrow(m)), to.x=rep(xy$x, each=nrow(m)), to.y=rep(xy$y, each=nrow(m)) )
    # d.m <- d.m[d.m$d > 0,]
    # lineare transformation of linewidth
    a <- 0.5
    b <- 10
    # d.m$d.sc <- (b-a) * (min(d.m$d)-a) + (b-a) /diff(range(d.m$d)) * d.m$d
    
    i <- .combPairs(1:dim(m)[1])
    d.m <- data.frame(from=colnames(m)[i[,1]], from=colnames(m)[i[, 2]], d=m[lower.tri(m)],
                      from.x=xy[[1]][i[,2]], to.x=xy[[1]][i[,1]],
                      from.y=xy[[2]][i[,2]], to.y=xy[[2]][i[,1]])
    
    if(is.null(lwd))
      d.m$d.sc <- .linScale(abs(d.m$d), newlow=a, newhigh=b )
    else
      d.m$d.sc <- lwd
    
    if(is.null(lwd))
      d.m$lty <- par("lty")
    else
      d.m$lty <- lty
    
    segments( x0=d.m$from.x, y0=d.m$from.y, x1 = d.m$to.x, y1 = d.m$to.y,
              col = col[((sign(d.m$d)+1)/2)+1], lty = d.m$lty, lwd=d.m$d.sc, lend= 1)
    points( xy, cex=pt.cex, pch=pch, col=pt.col, bg=pt.bg )
    
    # find min/max negative value and min/max positive value
    i <- c(which.min(d.m$d), which.max(ifelse(d.m$d<=0, d.m$d, NA)), which.min(ifelse(d.m$d>0, d.m$d, NA)), which.max(d.m$d))
    
    args.legend1 <- list( x="bottomright",
                          legend=gsub("0.", ".", format(d.m$d[i], digits=3), fixed = TRUE), 
                          lwd = d.m$d.sc[i],
                          col=rep(col, each=2), bg="white", cex=0.8)
    if ( !is.null(args.legend) ) { args.legend1[names(args.legend)] <- args.legend }
    add.legend <- TRUE
    if(!is.null(args.legend)) if(all(is.na(args.legend))) {add.legend <- FALSE}
    
    if(add.legend) do.call("legend", args.legend1)
  
    invisible(xy)
  
  })
}
