
#' Plot proportions for binary (logical) data with confidence intervals
#'
#' Creates a compact graphical representation of a binary outcome showing
#' the observed proportion together with binomial confidence intervals.
#' The plot is designed for objects containing absolute and relative
#' frequencies of two-level logical data.
#'
#' Optionally, confidence intervals at the 90%, 95%, and 99% levels are shown,
#' along with a legend explaining the interval widths.
#'
#' Graphical parameters supplied via \code{...} are applied globally using
#' \code{par()}.
#'
#' @param x An object containing frequency information for a binary variable.
#'   The object must provide absolute frequencies in \code{x$afrq} and
#'   relative frequencies in \code{x$rfrq}. Only two-level data are supported.
#'
#' @param main Plot title. If \code{NULL}, a title stored in \code{x} is used.
#'   If still \code{NULL}, the name of \code{x} is used.
#'   Use \code{NA} to suppress the title entirely.
#'
#' @param xlab Character string giving the label of the x-axis.
#'
#' @param col Vector of colors used for the plot elements. If \code{NULL},
#'   a default color palette is used. Colors are recycled as needed.
#'
#' @param legend Logical. If \code{TRUE} (default), a legend explaining the
#'   confidence intervals is drawn.
#'
#' @param xlim Numeric vector of length two giving the x-axis limits.
#'   Defaults to \code{c(0, 1)}.
#'
#' @param confint Logical. If \code{TRUE} (default), binomial confidence
#'   intervals at levels 0.90, 0.95, and 0.99 are displayed.
#'
#' @param ... Graphical parameters passed to \code{par()}, such as
#'   \code{cex}, \code{las}, \code{mar}, or \code{oma}.
#'
#' @details
#' The observed proportion is shown as a horizontal bar, with optional
#' confidence intervals overlaid as shaded rectangles. Confidence intervals
#' are computed as Wilson intervals.
#'
#' The title is drawn in the outer margin to ensure consistent alignment.
#'
#' @return Invisibly returns \code{NULL}. The function is called for its
#'   side effect of producing a plot.
#'
#' @seealso \code{\link[DescToolsX]{binomCI}}
#'
#' @examples
#' # Example Desc.logical object
#' x <- structure(list(
#'   xname = "some boolean", label = NULL, class = "logical", 
#'   classlabel = "logical", length = 50L, n = 50L, NAs = 0L, 
#'   main = "some boolean", plotit = TRUE, digits = NULL, unique = 2L, 
#'   afrq = structure(c(`FALSE` = 29L, `TRUE` = 21L), dim = 2L, 
#'                    dimnames = list(x = c("FALSE", "TRUE")), class = "table"), 
#'   rfrq = structure(c(0.58, 0.42, 0.442334417685783, 
#'                      0.29375003354712, 0.70624996645288, 
#'                      0.557665582314218), dim = 2:3, 
#'                    dimnames = list(c("FALSE", "TRUE"), c("est", "lci", "uci"))), 
#'   conf.level = 0.95), class = c("Desc.logical", "Desc")
#' ) 
#' 
#' plot(x)
#' plot(x, confint = FALSE)
#' plot(x, main = NA)  # suppress title
#'  



#' @export
plot.Desc.logical <- function(x, main = NULL, xlab = "", col = NULL,
                        legend = TRUE, xlim = c(0, 1), confint = TRUE,
                        ...) {
  
  
  .withGraphicsState({
    
    if (is.null(main)) 
      main <- main %||% 
                if(inherits(x, "Desc.logical")) x[["main"]] %||% 
                deparse(substitute(x))

    if (is.null(col)) {
      col <- c(DescToolsGraphics::Pal()[1:2], "grey80", "grey60", "grey40")
    } else {
      col <- rep(col, length.out = 5)
    }
    
    tab <- x$afrq
    ptab <- x$rfrq[, 1]
    if (nrow(x$rfrq) > 2) stop("!plot.Desc.logical! can only display 2 levels")
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    
    par(mar = c(4.1, 2.1, 0, 2.1))
    if (!is.na(main)) par(oma = c(0, 0, 3, 0))
    
    plot(
      x = ptab[1], y = 1, cex = 0.8, xlim = xlim, yaxt = "n", ylab = "",
      type = "n", bty = "n", xlab = xlab, main = NA
    )
    segments(x0 = 0, x1 = 1, y0 = 1, y1 = 1, col = "grey")
    segments(x0 = c(0, 1), x1 = c(0, 1), y0 = 0.8, y1 = 1.2, col = "grey")
    
    # insert grid
    segments(
      x0 = seq(0, 1, 0.1), x1 = seq(0, 1, 0.1), y0 = 0.8, y1 = 1.2,
      col = "grey", lty = "dotted"
    )
    rect(xleft = 0, ybottom = 0.95, xright = ptab[1], ytop = 1.05, col = col[1]) # greenyellow
    rect(xleft = ptab[1], ybottom = 0.95, xright = 1, ytop = 1.05, col = col[2]) # green4
    
    if (confint) {
      ci.99 <- .binomCI_raw(tab[1], sum(tab), conf.level = 0.99)[2:3]
      ci.95 <- .binomCI_raw(tab[1], sum(tab), conf.level = 0.95)[2:3]
      ci.90 <- .binomCI_raw(tab[1], sum(tab), conf.level = 0.90)[2:3]
      rect(xleft = ci.99[1], ybottom = 0.9, xright = ci.99[2], 
           ytop = 1.1, col = col[3]) # olivedrab1
      rect(xleft = ci.95[1], ybottom = 0.9, xright = ci.95[2], 
           ytop = 1.1, col = col[4]) # olivedrab3
      rect(xleft = ci.90[1], ybottom = 0.9, xright = ci.90[2], 
           ytop = 1.1, col = col[5]) # olivedrab4
      segments(x0 = ptab[1], x1 = ptab[1], y0 = 0.7, y1 = 1.3)
    }
    
    if (legend) {
      legend(
        x = 0, y = 0.75, legend = c("ci.99     ", "ci.95     ", "ci.90     "),
        box.col = "white",
        fill = col[3:5], bg = "white", cex = 1, ncol = 3,
        text.width = c(0.2, 0.2, 0.2)
      )
    }
    if (length(rownames(tab)) == 1) {
      text(rownames(tab), x = ptab[1] / 2, y = 1.2)
    } else {
      text(rownames(tab), x = c(ptab[1], ptab[1] + 1) / 2, y = 1.2)
    }
    
    if (!is.na(main)) title(main = main, outer = TRUE)
    
  })  
  
  invisible()
  
}