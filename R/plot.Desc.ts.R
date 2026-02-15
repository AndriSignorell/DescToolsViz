
#' Plot frequency tables with bar or dot representation
#'
#' Creates a ACF plot for objects produced by \code{Desc.ts()}.
#' Two mosaicplots are placed side by side
#'
#' Graphical parameters supplied via \code{...} are applied globally using
#' \code{par()} and affect all subplots consistently.
#'
#' @param x An object of class \code{Desc.table}.
#' @param ... Graphical parameters passed to \code{par()}, such as
#'   \code{cex}, \code{las}, \code{mar}, or \code{oma}.
#'
#'


#'@export
plot.Desc.ts <- function(x, ...){
  plotTimeSeries(x$x, ...)
}


