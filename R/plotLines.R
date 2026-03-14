

#' @export
plotLines <- function(x, y, col=1:5, xlab = NULL,
                      ylab = NULL, xlim = NULL, ylim = NULL, 
                      lty = 1, lwd = 2, lend = par("lend"),
                      xaxt=NULL, yaxt=NULL, 
                      cex = 1, legend = TRUE, 
                      main=NULL, grid=TRUE, pch=FALSE, ...){
  
  # example:
  #
  # m <- matrix(c(3,4,5,1,5,4,2,6,2), nrow = 3,
  #             dimnames = list(dose = c("A","B","C"),
  #                             age = c("2000","2001","2002")))
  # PlotLinesA(m, col=rev(c(PalHelsana(), "grey")), main="Dosw ~ age", lwd=3, ylim=c(1,10))
  
  
  .withGraphicsState({

    if(missing(y))
      z <- as.matrix(x)
    else
      z <- as.matrix(y)

    add.legend <- !(isNA(legend) %||% isFALSE(legend))
    
    # par() aus ...
    do.call(.applyParFromDots, 
            .mergeArgs(defaults=list(
                mar=c(right=10)), 
                list(...)
                ))
        
    
    last <- t(tail(apply(as.matrix(z), 2, .locf), 1))
    last <- sort(setNames(as.vector(last), nm=rownames(last)))
    

    if(!.inDots(..., arg = "add", default=FALSE)){
      # do not draw axes, labels and grid when only lines have to be added
      matplot(x, y, type="n", las=1, xlim=xlim, ylim=ylim, xaxt="n", 
              yaxt=yaxt, main=main, xlab=xlab, ylab=ylab, cex = cex, ...)
      if(!identical(xaxt, "n"))
        # use rownames for x-axis if available, but only if either x or y is missing
        if(!is.null(rownames(z)) && (missing(x) || missing(y)))
          axis(side = 1, at=c(1:nrow(z)), rownames(z))
      else
        axis(side=1)

      .callIf(graphics::grid, grid, 
              defaults = list(
                col   = "grey85",
                lty   = 1,
                lwd   = 1
              )  )

    }
    
    matplot(x, y, type="l", col=col, lty=lty, lwd=lwd, 
            xaxt="n", yaxt="n", add=TRUE, ...)
    
    # if(!is.na(pch))
    #   matplot(x, y, type="p", pch=pch, col=pch.col, bg=pch.bg, cex=pch.cex, 
    #           xaxt="n", yaxt="n", add=TRUE)
    
    
    # pch handling, if given
    
    pch.args <- list(
      x    = x,
      type = "p",
      pch  = 1,
      col  = par("fg"),
      bg   = par("bg"),
      cex  = 1,
      xaxt = "n",
      yaxt = "n",
      add  = TRUE
    )
    
    if (!missing(y)) {
      pch.args$y <- y
    }
    
    .callIf(matplot, pch, defaults = pch.args)
    

    
    if (add.legend) {
      
      if(is.null(colnames(z)))
        colnames(z) <- seq(ncol(z))
      
      ord <- match(names(last), colnames(z))
      lwd <- rep(lwd, length.out=ncol(z))
      lty <- rep(lty, length.out=ncol(z))
      col <- rep(col, length.out=ncol(z))

      .callIf(.legend, 
              legend,
              defaults=list(
                  line   = c(1, 1) ,   
                  width  = 1,          
                  labels = names(last), 
                  y      = spreadOut(unlist(last), 
                                     mindist = 1.2 * strheight("M") * (
                                       if (is.list(legend) && !is.null(legend$cex)) 
                                         legend$cex else par("cex") )),
                  cex    = par("cex"),
                  col = col[ord], lwd = lwd[ord], lty = lty[ord])
              )

    }
  
    invisible(list(x=x, y= if (!missing(y)) y else NULL, 
                   legend = if(add.legend) legend else NULL))
  
  })
  
  
}



# == internal helper functions =======================================


.locf <- function(x) {
  
  # last observation carried forward
  # replaces NAs by the last observed value
  
  l <- !is.na(x)
  rep(c(NA, x[l]), diff(c(1L, which(l), length(x) + 1L)))

  
}



.inDots <- function(..., arg, default){
  
  # was arg in the dots-args? parse dots.arguments
  arg <- unlist(match.call(expand.dots=FALSE)$...[arg])
  
  # if arg was not in ... then return default
  if(is.null(arg)) arg <- default
  
  return(arg)
  
}




.legend <- function(line, y, width, labels, lty, lwd, col, cex, main=NULL){

  par(xpd=TRUE)
  
  line <- rep(line, length.out=2)
  
  txtline <- line[1] + naReplace(width + (!is.na(width)) * line[2], 0)
  mtext(side = 4, las=1, cex=cex, text = labels,
        line = txtline,
        at = y
  )
  
  if(!is.na(width)){
    x0 <- lineToUser(line[1], 4)
    segments(x0 = x0, x1 = lineToUser(line[1] + width, 4), y0 = y,
             lwd = lwd, lty=lty, lend = 1, col = col)
  }
  
  if(!is.null(main))
    mtext(side=4, text = main, las=1, line=line[1], at=par("usr")[4], padj=c(0))
}


