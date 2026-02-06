

# internal getOption wrapper for DescTools options
.getOption <- function(name, default = NULL) {
  getOption(paste0("DescToolsX.", name), default)
}





# internal function to restore settings after a plot has been created

.withGraphicsState <- function(expr) {
  
  op <- par(no.readonly = TRUE)
  ok <- FALSE
  
  on.exit({
    layout(matrix(1))
    par(op)
    
    if (ok && !is.null(.getOption("stamp"))) {
      tryCatch(stamp(), error = function(e) NULL)
    }
    
  }, add = TRUE)
  
  force(expr)
  ok <- TRUE
  
}



.resolvePar <- function(name, value = NULL, default = NULL) {
  
  if (!is.null(value)) {
    return(value)
  }
  
  opt <- .getOption(paste0("descToolsX.plot.", name))
  if (!is.null(opt)) {
    return(opt)
  }
  
  default
}


.applyParFromDots <- function(...) {
  
  dots <- list(...)
  if (!length(dots)) return(invisible())
  
  valid <- names(par(no.readonly = TRUE))
  dots <- dots[names(dots) %in% valid]
  
  if (length(dots)) {
    do.call(par, dots)
  }
  
  invisible()
}





