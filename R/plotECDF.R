
#' Empirical Cumulative Distribution Function 
#' 
#' Faster alternative for plotting the empirical cumulative distribution
#' function (ecdf). The function offers the option to construct the ecdf on the
#' base of a histogram, which makes sense, when x is large. So the plot process
#' is much faster, without loosing much precision in the details. 
#' 
#' The stats function \code{\link{plot.ecdf}} is fine for vectors that are not
#' too large. However for n ~ 1e7 we would observe a dramatic performance
#' breakdown (possibly in combination with the use of \code{\link{do.call}}).
#' 
#' \code{PlotECDF} is designed as alternative for quicker plotting the ecdf for
#' larger vectors. If \code{breaks} are provided as argument, a histogram with
#' that number of breaks will be calculated and the ecdf will use those
#' frequencies instead of respecting every single point. \cr Note that a plot
#' will rarely need more than ~1'000 points on x to have a sufficient
#' resolution on usual terms. \code{\link{plotFdist}} will also use this number
#' of breaks by default. 
#' 
#' @param x numeric vector of the observations for ecdf. 
#' @param breaks will be passed directly to \code{\link{hist}}. If left to
#' \code{NULL}, no histogram will be used. 
#' @param col color of the line. 
#' @param ylab label for the y-axis. 
#' @param lwd line width. 
#' @param xlab label for the x-axis. 
#' @param \dots arguments to be passed to subsequent functions. 
#' 
#' @return no value returned, use \code{\link{plot.ecdf}} if any results are
#' required. 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{plot.ecdf}}, \code{\link{plotFdist}} 
#' @keywords hplot
#' @examples
#' 
#' plotECDF(faithful$eruptions)
#' 
#' # make large vector
#' x <- rnorm(n=1e7)
#' 
#' # plot only 1000 points instead of 1e7
#' plotECDF(x, breaks=1000)


#' @export
plotECDF <- function(x, breaks=NULL, col=Pal()[1],
                     ylab="", lwd = 2, xlab = NULL, ...){
  
  if(is.null(breaks)){
    tab <- table(x)
    xp <-  as.numeric(names(tab))
    xp  <- c(head(xp,1), xp)
    yp <- c(0, cumsum(tab))
  } else {
    xh <- hist(x, breaks=breaks, plot=FALSE)
    xp <- xh$mids
    xp  <- c(head(xp,1), xp)
    yp <- c(0, cumsum(xh$density))
  }
  yp <- yp * 1/tail(yp, 1)
  
  if(is.null(xlab)) 
    xlab <- deparse(substitute(x))
  
  plot(yp ~ xp, lwd=lwd, type = "s", col=col, xlab= xlab, yaxt="n",
       ylab = "", panel.first=quote(grid(ny = NA)), ...)
  
  # we must not pass all dot arguments to axis and plot, as plot accepts arguments
  # which axis does not (e.g. frame.plot) and consequently barks
  # so we select all arguments from axis, combine them with par (which will presumably be ok -- really all par???)
  # and filter them from the whole args list
  
  # ... nice try, but far too many non valid args:  
  # validargs <- names(subset(validargs <- c(as.list(args(axis)), 
  #                                          par(no.readonly = TRUE)), 
  #                           subset = names(validargs) %nin% c("...","")))      # omit ... and empty
  
  validargs <- subset(validargs <- c(names(as.list(args(axis))),
                                     c("cex", "cex.axis", "col.axis", "family", 
                                       "fg", "font", "font.axis", "las", "mgp", 
                                       "srt", "tck", "tcl", "yaxp", "yaxs", "yaxt")),
                      subset = validargs %.nin% c("...","","col"))      # omit ... and empty

  # the defaults
  axargs1 <- list(side = 2, at = seq(0, 1, 0.25),
                           # fm(seq(0, 1, 0.25), ldigits = 0, digits=2),
                  labels = c(".00", ".25", ".50", ".75", "1.00"),
                  las = 1, xaxs = "e", lwd.axis=1) 
  
  axargs1 <- .clearArgs(provided = c(as.list(environment()), list(...)),  # all provided arguments and their values 
                       valid=validargs,                                  # vector or names with all validargs
                       default = axargs1)
  axargs1[["lwd"]] <- axargs1[["lwd.axis"]]
  axargs1[["lwd.axis"]] <- NULL                                     # rename lwd, so we can use ... to supply a lwd for axis
  do.call(axis, axargs1)
  
  abline(h = c(0, 0.25, 0.5, 0.75, 1), 
         col = "grey", lty = c("dashed","dotted","dotted","dotted","dashed"))
  
  # mark min-max value
  points(x=range(x), y=c(0, 1), col=col,  pch=3, cex=2)
  
  if(!is.null(.getOption("stamp")))
    stamp()
  
}




.clearArgs <- function(provided, valid, default) {
  
  # we might want to use dots in a function for multiple functions
  # and extract only those arguments, which are accepted by a specific function
  # further we might have some defaults already defined
  # this function returns all valid provided arguments, extended by set defaults
  
  provided <- provided[names(provided) %in% valid]
  
  # the defaults
  args1 <- default
  
  # overwrite defaults with potentially provided values 
  args1[names(provided) %in% names(args1)] <- provided[names(provided) %in% names(args1)]
  
  # append all provided, already validated args, which were not defined as default
  args1 <- c(args1, provided[names(provided) %in% setdiff(provided, names(args1))])               
  
  # supply only the valid provided or default arguments to axis function 
  args1[names(provided)] <- provided
  
  # the cleared arguments
  return(args1)
  
}



