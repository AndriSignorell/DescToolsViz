
#' Plot frequency tables with bar or dot representation
#'
#' Creates a frequency plot for objects produced by \code{Freq()}.
#' Frequencies and percentages are displayed side by side using either
#' bar charts or dot plots. Optionally, a cumulative distribution
#' (ECDF-like display) can be added.
#'
#' Graphical parameters supplied via \code{...} are applied globally using
#' \code{par()} and affect all subplots consistently.
#'
#' @param x An object of class \code{"Freq"}, typically returned by
#'   \code{DescToolsX::freq()}.
#'
#' @param main Plot title. If \code{NULL}, the title stored in \code{x} is used.
#'   Use \code{NA} to suppress the title entirely.
#'
#' @param maxlablen Maximum number of characters used for category labels.
#'   Longer labels are truncated.
#'
#' @param type Character string specifying the plot type.
#'   Either \code{"bar"} (default) for bar charts or \code{"dot"} for dot plots.
#'
#' @param col Fill color(s) for bars or points. If \code{NULL}, sensible
#'   defaults are chosen automatically.
#'
#' @param border Border color for bars or points. If \code{NULL}, a default
#'   is used depending on \code{type}.
#'
#' @param xlim Numeric vector of length two giving the x-axis limits.
#'   If \code{NULL}, limits are chosen automatically.
#'
#' @param ecdf Logical. If \code{TRUE} (default) and \code{type = "bar"},
#'   cumulative percentages are shown in addition to individual percentages.
#'
#' @param ... Graphical parameters passed to \code{par()}, such as
#'   \code{cex}, \code{las}, \code{mar}, or \code{oma}.
#'
#' @details
#' The function uses a two-column layout: frequencies are displayed on the
#' left and percentages on the right. If the number of categories exceeds
#' the maximum specified in the \code{Freq()} object, the output is truncated
#' and a note is added to the plot.
#'
#' The title is drawn in the outer margin to ensure consistent alignment
#' across subplots.
#'
#' @return Invisibly returns \code{NULL}. The function is called for its
#'   side effect of producing a plot.
#'
#' @seealso \code{\link[DescTools]{Freq}}, \code{\link[DescTools]{Desc.factor}}
#'
#' @examples
#' # Example Desc.factor object
#' x <- structure(list(
#'   meta = list(main="myTitle",
#'               plotit = TRUE,
#'               label = NULL, 
#'               class = "character", 
#'               classlabel = "character"),
#'   xname = "sample(letters[1:4], size = 50, replace = TRUE)", 
#'   length = 50L, n = 50L, NAs = 0L, 
#'   digits = NULL, levels = 0L, 
#'   unique = 4L, dupes = TRUE, maxrows = 12, ord = "desc", 
#'   freq = structure(list(
#'     level = c("d", "c", "a", "b"), freq = c(19L, 15L, 10L, 6L), 
#'     perc = c(0.38, 0.3, 0.2, 0.12), cumfreq = c(19L, 34L, 44L, 50L), 
#'     cumperc = c(0.38, 0.68, 0.88, 1)), 
#'     class = c("Freq", "data.frame"), 
#'     row.names = c(NA, -4L), medianclass = "c")), class = c("Desc.factor", "Desc")
#'   ) 
#'         
#' plot(x)
#' plot(x, type = "dot", col = "steelblue")
#' plot(x, main = NA)  # suppress title
#'



#' @export
plot.Desc.factor <- function(x, main = NULL, maxlablen = 25,
                      type = c("bar", "dot"),
                      col = NULL, border = NULL, xlim = NULL, 
                      ecdf = TRUE, ...) {
  
  
  .withGraphicsState({
    
    # was cex in the dots-args? parse dots.arguments
    cex <- unlist(match.call(expand.dots = FALSE)$...["cex"])
    if (is.null(cex)) cex <- par("cex")
    
    tab <- as.table(x$freq$freq)
    names(tab) <- x$freq[[1]]
    ptab <- as.table(x$freq$perc)
    trunc_fg <- (nrow(tab) > x$maxrows)
    if (!is.na(x$maxrows) && x$maxrows < nrow(tab)) {
      tab <- tab[1:min(nrow(tab), x$maxrows)]
      ptab <- ptab[1:min(nrow(tab), x$maxrows)]
    }
    
    if (max(nchar(names(tab))) > maxlablen) {
      names(tab) <- strTrunc(names(tab), maxlablen)
    }
    wtxt <- max(strwidth(names(tab), "inch"))
    wplot <- (par("pin")[1] - wtxt) / 2
    layout(matrix(c(1, 2), nrow = 1), widths = c(wtxt + wplot, wplot) * 2.54)
    par(mai = c(1.2, max(strwidth(rev(names(tab)), "inch")) + .5, 0.2, .3) + .02)
    if (!is.na(x$meta$main)) par(oma = c(0, 0, 3, 0))
    
    
    switch(match.arg(arg = type, choices = c("bar", "dot")),
           dot = {
             if (is.null(xlim)) {
               xlim <- range(pretty(tab)) + c(-1, 1) * diff(range(pretty(tab))) * 0.04
             }
             
             if (is.null(col)) col <- DescToolsGraphics::Pal()[1]
             if (is.null(border)) border <- "black"
             b <- barplot(rev(tab),
                          horiz = TRUE, border = NA, col = "white", las = 1,
                          xlim = xlim,
                          xpd = FALSE, xlab = "frequency",
                          cex.names = cex, cex.axis = cex, cex.lab = cex, tck = -0.04
             )
             abline(h = b, v = 0, col = "grey", lty = "dotted")
             segments(0, b, as.vector(rev(tab)), b)
             points(
               x = as.vector(rev(tab)), y = b, yaxt = "n",
               col = border, pch = 21, bg = col, cex = 1.3
             )
             box()
             
             par(mai = c(1.2, 0.1, 0.2, .3) + .02)
             b <- barplot(rev(ptab),
                          horiz = TRUE, border = NA, col = "white", las = 1, names = "",
                          xlim = c(-0.04, 1.04),
                          xlab = "percent", cex.names = cex, cex.axis = cex,
                          cex.lab = cex, tck = -0.04
             )
             abline(h = b, v = 0, col = "grey", lty = "dotted")
             segments(0, b, as.vector(rev(ptab)), b)
             points(
               x = as.vector(rev(ptab)), y = b, col = border, pch = 21,
               bg = col, cex = 1.3
             )
             box()
           },
           bar = { # type = "bar"
             
             if (is.null(xlim)) {
               xlim <- range(pretty(c(0.96 * min(tab), 1.04 * max(tab))))
             }
             
             if (is.null(col)) {
               col <- c(
                 rep("grey80 ", length.out = 2 * nrow(tab)),
                 rep(DescToolsGraphics::alpha("grey80", 0.4), length.out = nrow(tab))
               )
             } else {
               if (length(col) == 1) {
                 col <- c(
                   rep(col, length.out = 2 * nrow(tab)),
                   rep(DescToolsGraphics::alpha(col, 0.3), length.out = nrow(tab))
                 )
               } else {
                 col <- rep(col, length.out = 3 * nrow(tab))
               }
             }
             if (is.null(border)) border <- NA
             barplot(rev(tab),
                     horiz = TRUE, col = col[1:nrow(tab)],
                     border = border, las = 1, xlim = xlim,
                     xpd = FALSE, xlab = "frequency",
                     cex.names = cex, cex.axis = cex, cex.lab = cex, tck = -0.04
             )
             grid(ny = NA)
             
             par(mai = c(1.2, 0.15, 0.2, .3) + .02)
             if (ecdf) {
               barplot(rev(cumsum(ptab)),
                       horiz = TRUE, col = col[(2 * nrow(tab) + 1):(3 * nrow(tab))],
                       border = border, las = 1,
                       names = "", xlim = c(0, 1), xlab = "percent",
                       cex.names = cex, cex.axis = cex, cex.lab = cex, tck = -0.04
               )
               barplot(rev(ptab),
                       horiz = TRUE, col = col[(nrow(tab) + 1):(2 * nrow(tab))],
                       border = border, names = "", xlab = NA, ylab = NA,
                       add = TRUE, axes = FALSE
               )
             } else {
               barplot(rev(ptab),
                       horiz = TRUE, col = col[(nrow(tab) + 1):(2 * nrow(tab))],
                       border = border, las = 1, names = "",
                       xlim = c(0, 1), xlab = "percent", cex.names = cex,
                       cex.axis = cex, cex.lab = cex, tck = -0.04
               )
             }
             grid(ny = NA)
           }
    )
    
    
    if (is.null(main)) main <- x$meta$main
    if (!is.na(main)) {
      title(main = main %||% x$meta$main, outer = TRUE)
    }
    
    if (trunc_fg) {
      text(
        x = par()$usr[2], y = 0.4, labels = " ...[list output truncated]  ",
        cex = 0.6, adj = c(1, 0.5)
      )
    }
    
    # if (!is.null(.getOption("stamp"))) {
    #   DescToolsGraphics::stamp()
    # }
    
    # close .withGraphicsState
  })
  
  invisible()
}

