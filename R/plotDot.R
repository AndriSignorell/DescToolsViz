
#' Dot Plot with Optional Confidence Intervals
#'
#' Draws a dot plot for numeric values with optional confidence intervals.
#' The function accepts vectors, matrices, or 3-dimensional arrays. Internally
#' the data are normalized to an array of dimension
#' \code{items × (est, low, high) × groups}.
#'
#' If lower and upper bounds are supplied, horizontal confidence intervals are
#' drawn with capped ends. Items are arranged vertically and may optionally be
#' grouped.
#'
#' @param x Numeric data. Can be
#'   \itemize{
#'     \item a numeric vector (estimates only),
#'     \item a matrix with 1 column (estimates) or 3 columns (estimate, lower,
#'       upper),
#'     \item a 3D array of dimension
#'       \code{items × (est, low, high) × groups}.
#'   }
#' @param items Optional labels for the items (rows).
#'   Defaults to \code{dimnames(x)[[1]]} if available.
#' @param groups Optional group labels. Defaults to
#'   \code{dimnames(x)[[3]]} if present.
#' @param main Main title of the plot.
#' @param xlim Limits for the horizontal axis.
#' @param gap Vertical spacing between groups.
#' @param axes Logical; if \code{TRUE} axes are drawn.
#' @param xax Optional specification of the x-axis. Passed to
#'   \code{\link{.drawAxis}}.
#' @param yax Optional specification of the y-axis.
#' @param box Logical; if \code{TRUE} a box is drawn around the plotting region.
#' @param grid Logical; if \code{TRUE} horizontal grid lines are drawn.
#' @param pch Plotting symbol specification for the points. May be a single
#'   value or a list of graphical parameters passed to
#'   \code{\link[graphics]{points}}.
#' @param col Colour used for points and confidence intervals.
#' @param ... Additional graphical parameters passed to \code{\link{par}} via
#'   \code{.applyParFromDots()}.
#'
#' @details
#' Graphical defaults may also be controlled globally through
#' \code{options(DescToolsX.theme = list(...))}. Values supplied as arguments
#' take precedence over theme settings.
#'
#' @return Invisibly returns a list with the vertical layout positions used in
#' the plot:
#' \itemize{
#'   \item \code{ypos} positions of items,
#'   \item \code{group_y} positions of group labels,
#'   \item \code{sep_y} positions of group separator lines.
#' }
#'
#' @examples
#' # simple dot plot
#' plotDot(c(12, 18, 28, 40, 65), items = LETTERS[1:5])
#'
#' # dot plot with confidence intervals
#' est  <- c(12, 18, 28, 40, 65)
#' low  <- est - 3
#' high <- est + 3
#'
#' plotDot(cbind(est, low, high), items = LETTERS[1:5])
#'
#' # grouped example
#' dat <- array(c(12,18,28,40,65,
#'                20,15,22,32,55),
#'              dim = c(5,1,2))
#'
#' plotDot(dat,
#'         items = LETTERS[1:5],
#'         groups = c("Group A","Group B"))
#'
#' plotDot(
#'   dat, main="Plot Dot VADeaths", cex.axis=0.8,
#'   items = c("50-54","55-59","60-64","65-69","70-74"),
#'   groups = c("Rural Male","Rural Female","Urban Male","Urban Female"),
#'   xlim = c(0,80),
#'   # axes=F,
#'   # xax = list(fmt="%", d=0, big=",", col="red", tck=-0.05),
#'   pch = list(pch=c(16, 21), col= c("green","blue"), cex=c(1,2), bg="white")
#'   # col = c("green","blue","orange","magenta")
#' )
#' 
#' 
#' plotDot(c(12,18,28,40,65), # groups="", 
#'         items=LETTERS[1:5], pch=list(cex=1.5), main="Title")
#' 
#' 
#' plotDot(
#'   cbind(est[, 1], low[, 1], high[,1]), 
#'   items=LETTERS[1:5])
#'   


#' @export
plotDot <- function(x, 
                    items = NULL,
                    groups = NULL,
                    main=NULL, 
                    xlim = NULL,
                    gap = 1,
                    axes = TRUE,
                    xax = NULL, 
                    yax = NULL, 
                    box = TRUE,
                    grid = TRUE,
                    pch = NULL, 
                    # col = NULL,
                    ...) {

  
  th <- .theme(
    
    grid = grid,
    pch  = pch
  )
  
  
  grid <- th$grid
  pch  <- th$pch
  
  
  x <- .normalizeDotData(x)
  
  nm <- .resolveNames(x, items, groups)
  items  <- nm$items
  groups <- nm$groups
  
  if (dim(x)[3] == 1 && missing(groups))
    groups <- NULL

  
  .withGraphicsState({

    .applyParFromDots(...)
    
    if(length(dim(x)) != 3)
      stop("x must be age x (est,low,high) x group")
    
    nx <- dim(x)[1]
    ng   <- dim(x)[3]
    
    drawGroupHeader <- ng > 1 
    header <- if (drawGroupHeader) 1 else 0
    
    if(is.null(items))
      items <- seq_len(nx)
    
    if(is.null(groups))
      groups <- paste("Group", seq_len(ng))
    
    if(is.null(xlim))
      xlim <- range(x, na.rm = TRUE)
    
    # --------------------------------
    # Margin automatisch anpassen
    # --------------------------------
    
    .adjustLeftMarginForLabels(c(groups, items), pad=1)
    
    
    # --------------------------------
    # Y layout
    # --------------------------------
    
    ypos    <- vector("list", ng)
    sep_y   <- numeric(ng)
    group_y <- numeric(ng)
    
    base <- 0
    
    for(g in seq_len(ng)) {
      
      # ypos[[g]]    <- base + seq_len(nx)
      ypos[[g]]    <- base + rev(seq_len(nx))
      sep_y[g]     <- base + nx + header
      group_y[g]   <- base + nx + header
      
      base <- base + nx + gap + header
    }
    
    ymax <- base - gap - header
    
    
    # --------------------------------
    # Plot
    # --------------------------------
    
    plot.new()
    
    plot.window(
      xlim = xlim,
      ylim = c(0, ymax + 1 + header),
      xaxs = "r",
      yaxs = "i"
    )
    
    usr <- par("usr")
    
    
    # --------------------------------
    # Grid
    # --------------------------------

    .callIf(
      .drawDotGrid,
      grid,
      defaults = list(
        ypos = ypos,
        sep_y = sep_y,
        drawGroupHeader = drawGroupHeader,
        col = th$grid.col,
        lty = th$grid.lty,
        lwd = th$grid.lwd,
        group.col = th$grid.group.col,
        group.lty = th$grid.group.lty,
        group.lwd = th$grid.group.lwd
      )
    )
    
    
    # --------------------------------
    # Axes
    # --------------------------------
    
    if(isTRUE(axes)) {
      
      .drawAxis(1, xax)

      axis(
        2,
        at = unlist(ypos),
        labels = rep(items, ng),
        las = 1
      )
    }
    
    
    # --------------------------------
    # Group labels
    # --------------------------------
    if (drawGroupHeader) {
      
      x_left <- usr[1] - diff(usr[1:2]) * 0.03
      
      for(g in seq_len(ng)) {
        
        text(
          x_left,
          group_y[g],
          groups[g],
          adj = c(1,0.5),
          xpd = NA,
          font = 2,
          cex = par("cex.axis")
        )
      }
    }
    
    # draw box if box != FALSE || NA
    .callIf(graphics::box, 
            box,
            defaults=list(which="plot"))
    
    # place main title if main != FALSE || NA
    if(!(is.null(main) %||% main=="" %||% isNA(main)))
      title(main=main)
    
 
    # add data
    .addDotCI(
      x,
      ypos,
      pch = pch
    )

        
  })
  
  invisible(list(
    ypos = ypos,
    group_y = group_y,
    sep_y = sep_y
  ))
  
}



.addDotCI <- function(x, ypos, pch = list(pch = 16), lwd = 1) {
  
  if (!is.list(ypos))
    stop("ypos must be list of y-vectors")
  
  ng <- dim(x)[3]
  
  # ensure pch is a list
  if (!is.list(pch))
    pch <- list(pch = pch)
  
  for (g in seq_len(ng)) {
    
    est  <- x[,1,g]
    low  <- x[,2,g]
    high <- x[,3,g]
    
    y <- as.numeric(ypos[[g]])
    
    # CI lines
    graphics::arrows(
      low, y, high, y,
      col   = pch$col %||% par("fg"),
      lwd   = lwd,
      code  = 3,
      angle = 90,
      length = 0.05
    )
    
    # points
    .callIf(
      graphics::points,
      pch,
      defaults = list(
        x   = est,
        y   = y,
        pch = rep_len(pch$pch %||% 16, ng)[g],
        col = rep_len(pch$col %||% par("fg"), ng)[g],
        bg  = rep_len(pch$bg  %||% NA, ng)[g],
        cex = rep_len(pch$cex %||% 1, ng)[g]
      ),
      forbidden = c("x", "y")
    )
    
  }
  
  invisible(NULL)
}



.resolveNames <- function(x, items=NULL, groups=NULL) {
  
  dn <- dimnames(x)
  
  if (is.null(items)) {
    items <- if (!is.null(dn[[1]])) dn[[1]] else seq_len(dim(x)[1])
  }
  
  if (is.null(groups)) {
    groups <- if (!is.null(dn[[3]])) dn[[3]] else seq_len(dim(x)[3])
  }
  
  list(items=items, groups=groups)
}




.normalizeDotData <- function(x) {
  
  if (is.array(x) && length(dim(x)) == 3)
    return(x)
  
  # ----------------------------
  # vector -> estimate only
  # ----------------------------
  
  if (is.vector(x)) {
    
    out <- array(NA_real_, dim=c(length(x),3,1))
    out[,1,1] <- x
    
    return(out)
  }
  
  # ----------------------------
  # matrix
  # ----------------------------
  
  if (is.matrix(x)) {
    
    n <- nrow(x)
    
    if (ncol(x) == 1) {
      
      out <- array(NA_real_, dim=c(n,3,1))
      out[,1,1] <- x[,1]
      
      return(out)
      
    } else if (ncol(x) == 3) {
      
      out <- array(NA_real_, dim=c(n,3,1))
      out[,,1] <- x
      
      return(out)
    }
    
    stop("Matrix must have 1 or 3 columns")
  }
  
  stop("Unsupported data structure")
}


.drawDotGrid <- function(
    ypos, sep_y, drawGroupHeader,
    col = "orange", lty = 3, lwd=1,
    group.col = "grey40", group.lty = 2, group.lwd=1
) {
  
  ng <- length(ypos)
  
  for (g in seq_len(ng)) {
    
    graphics::abline(h = ypos[[g]], col = col, lty = lty)
    
    if (drawGroupHeader)
      graphics::abline(h = sep_y[g], col = group.col, lty = group.lty)
  }
  
  invisible(NULL)
}

