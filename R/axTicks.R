
#' Compute Axis Tickmark Locations (For POSIXct Axis) 
#' 
#' Compute pretty tickmark locations, the same way as R does internally. By
#' default, gives the at values which axis.POSIXct(side, x) would use. 
#' 
#' \link{axTicks} has no implementation for POSIXct axis. This function fills
#' the gap. 
#' 
#' @name axTicks
#' @aliases axTicks.POSIXct axTicks.Date
#' @param side See \link{axis}. 
#' @param x,at A date-time or date object. 
#' @param format See strptime. 
#' @param labels Either a logical value specifying whether annotations are to
#' be made at the tickmarks, or a vector of character strings to be placed at
#' the tickpoints. 
#' @param \dots Further arguments to be passed from or to other methods. 
#' 
#' @return numeric vector of coordinate values at which axis tickmarks can be
#' drawn. 
#' 
#' @author Andri Signorell <andri@@signorell.net> simply copying R-Core code 
#' @seealso \code{\link{axTicks}}, \code{\link{axis.POSIXct}} 
#' @keywords aplot chron
#' @examples
#' 
#' with(beaver1, {
#'   time <- strptime(paste(1990, day, time %/% 100, time %% 100),
#'                    "%Y %j %H %M")
#'   plot(time, temp, type = "l") # axis at 4-hour intervals.
#'   # now label every hour on the time axis
#'   plot(time, temp, type = "l", xaxt = "n")
#'   r <- as.POSIXct(round(range(time), "hours"))
#'   axis.POSIXct(1, at = seq(r[1], r[2], by = "hour"), format = "%H")
#'   # place the grid
#'   abline(v=axTicks.POSIXct(1, at = seq(r[1], r[2], by = "hour"), format = "%H"),
#'          col="grey", lty="dotted")
#' })
#' 

#' @rdname axTicks
#' @export
axTicks.POSIXct <- function (side, x, at, format, labels = TRUE, ...) {
  
  # This is completely original R-code with one exception:
  # Not an axis is drawn but z are returned.
  
  mat <- missing(at) || is.null(at)
  if (!mat)
    x <- as.POSIXct(at)
  else x <- as.POSIXct(x)
  range <- par("usr")[if (side %% 2L)
    1L:2L
    else 3L:4L]
  d <- range[2L] - range[1L]
  z <- c(range, x[is.finite(x)])
  attr(z, "tzone") <- attr(x, "tzone")
  if (d < 1.1 * 60) {
    sc <- 1
    if (missing(format))
      format <- "%S"
  }
  else if (d < 1.1 * 60 * 60) {
    sc <- 60
    if (missing(format))
      format <- "%M:%S"
  }
  else if (d < 1.1 * 60 * 60 * 24) {
    sc <- 60 * 60
    if (missing(format))
      format <- "%H:%M"
  }
  else if (d < 2 * 60 * 60 * 24) {
    sc <- 60 * 60
    if (missing(format))
      format <- "%a %H:%M"
  }
  else if (d < 7 * 60 * 60 * 24) {
    sc <- 60 * 60 * 24
    if (missing(format))
      format <- "%a"
  }
  else {
    sc <- 60 * 60 * 24
  }
  if (d < 60 * 60 * 24 * 50) {
    zz <- pretty(z/sc)
    z <- zz * sc
    z <- .POSIXct(z, attr(x, "tzone"))
    if (sc == 60 * 60 * 24)
      z <- as.POSIXct(round(z, "days"))
    if (missing(format))
      format <- "%b %d"
  }
  else if (d < 1.1 * 60 * 60 * 24 * 365) {
    z <- .POSIXct(z, attr(x, "tzone"))
    zz <- as.POSIXlt(z)
    zz$mday <- zz$wday <- zz$yday <- 1
    zz$isdst <- -1
    zz$hour <- zz$min <- zz$sec <- 0
    zz$mon <- pretty(zz$mon)
    m <- length(zz$mon)
    M <- 2 * m
    m <- rep.int(zz$year[1L], m)
    zz$year <- c(m, m + 1)
    zz <- lapply(zz, function(x) rep(x, length.out = M))
    zz <- .POSIXlt(zz, attr(x, "tzone"))
    z <- as.POSIXct(zz)
    if (missing(format))
      format <- "%b"
  }
  else {
    z <- .POSIXct(z, attr(x, "tzone"))
    zz <- as.POSIXlt(z)
    zz$mday <- zz$wday <- zz$yday <- 1
    zz$isdst <- -1
    zz$mon <- zz$hour <- zz$min <- zz$sec <- 0
    zz$year <- pretty(zz$year)
    M <- length(zz$year)
    zz <- lapply(zz, function(x) rep(x, length.out = M))
    z <- as.POSIXct(.POSIXlt(zz))
    if (missing(format))
      format <- "%Y"
  }
  if (!mat)
    z <- x[is.finite(x)]
  keep <- z >= range[1L] & z <= range[2L]
  z <- z[keep]
  if (!is.logical(labels))
    labels <- labels[keep]
  else if (identical(labels, TRUE))
    labels <- format(z, format = format)
  else if (identical(labels, FALSE))
    labels <- rep("", length(z))
  
  # axis(side, at = z, labels = labels, ...)
  # return(list(at=z, labels=labels))
  return(z)
}



#' @rdname axTicks
#' @export
axTicks.Date <- function(side = 1, x, ...) {
  ##  This functions is almost a copy of axis.Date
  x <- as.Date(x)
  range <- par("usr")[if (side%%2)
    1L:2L
    else 3:4L]
  range[1L] <- ceiling(range[1L])
  range[2L] <- floor(range[2L])
  d <- range[2L] - range[1L]
  z <- c(range, x[is.finite(x)])
  class(z) <- "Date"
  if (d < 7)
    format <- "%a"
  if (d < 100) {
    z <- structure(pretty(z), class = "Date")
    format <- "%b %d"
  }
  else if (d < 1.1 * 365) {
    zz <- as.POSIXlt(z)
    zz$mday <- 1
    zz$mon <- pretty(zz$mon)
    m <- length(zz$mon)
    m <- rep.int(zz$year[1L], m)
    zz$year <- c(m, m + 1)
    z <- as.Date(zz)
    format <- "%b"
  }
  else {
    zz <- as.POSIXlt(z)
    zz$mday <- 1
    zz$mon <- 0
    zz$year <- pretty(zz$year)
    z <- as.Date(zz)
    format <- "%Y"
  }
  keep <- z >= range[1L] & z <= range[2L]
  z <- z[keep]
  z <- sort(unique(z))
  class(z) <- "Date"
  z
}




