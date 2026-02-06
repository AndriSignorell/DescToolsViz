

# internal function to restore settings after a plot has been created

.withGraphicsState <- function(expr) {
  
  op <- par(no.readonly = TRUE)
  on.exit({
    layout(matrix(1))
    par(op)
  }, add = TRUE)
  
  force(expr)
  
}

# internal getOption wrapper for DescTools options
.getOption <- function(name, default = NULL) {
  getOption(paste0("DescToolsX.", name), default)
}


