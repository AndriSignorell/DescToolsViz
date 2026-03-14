

# internal getOption wrapper for DescTools options
.getOption <- function(name, default = NULL) {
  getOption(paste0("DescToolsX.", name), default)
}





.withGraphicsState <- function(expr, stamp = .getOption("stamp", NULL)) {
  
  # nur "sichere" par-Parameter speichern
  keep <- c(
    "mar","mai","cex","cex.axis","cex.lab","cex.main","cex.sub",
    "las","tck","mgp","xaxs","yaxs","xaxt","yaxt",
    "col","col.axis","col.lab","col.main","col.sub",
    "lwd","lty","pch","bg","fg","xpd", "plt"
  )
  
  op <- par(keep)
  
  withr::defer(par(op))
  withr::local_options(warn = 1)
  
  ok <- FALSE
  
  on.exit({
    if (ok && !is.null(stamp))
      tryCatch(stamp(), error = function(e) NULL)
  }, add = TRUE)
  
  eval.parent(substitute(expr))
  
  ok <- TRUE
  invisible(NULL)
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



.isLastPanel <- function(tol = 1e-7) {
  # returns TRUE if the current panel is the last on mfrow, layout screens
  
  if (par("page"))
    return(FALSE)
  
  mfg <- par("mfg")
  
  if(length(mfg) == 4)
    return(mfg[1] == mfg[3] && mfg[2] == mfg[4])
  
  # needed for split.screen or manuel set fig parameters
  fig <- par("fig")
  abs(fig[2] - 1) < tol && abs(fig[3] - 0) < tol
}



.applyParFromDots <- function(...) {

  # if some defaults for dots required use:
  # do.call(.applyParFromDots, 
  #         .mergeArgs(defaults=list(
  #           yaxs="r"), 
  #           list(...)
  #         ))
  
  
  patch_fourpar <- function(new_val, old_val, pname) {
    
    if (!is.null(names(new_val))) {
      
      idx <- match(names(new_val),
                   c("bottom","left","top","right"))
      
      if (any(is.na(idx)))
        stop(sprintf("%s names must be bottom, left, top, right", pname))
      
      old_val[idx] <- new_val
      return(old_val)
    }
    
    new_val <- rep_len(new_val, 4)
    idx_na <- is.na(new_val)
    new_val[idx_na] <- old_val[idx_na]
    
    new_val
  }
  
    
  dots <- list(...)
  if (!length(dots)) return(invisible())
  
  dots <- dots[!is.na(names(dots))]
  dots <- dots[names(dots) %in% names(par(no.readonly = TRUE))]
  
  if (!length(dots)) return(invisible())
  
  p <- par(no.readonly = TRUE)
  
  
  if ("mar" %in% names(dots)) {
    dots$mar <- patch_fourpar(dots$mar, p$mar, "mar")
  }
  
  if ("oma" %in% names(dots)) {
    dots$oma <- patch_fourpar(dots$oma, p$oma, "oma")
  }
  
  do.call(par, dots)
  
  invisible()
}



.adjustLeftMarginForLabels <- function(labels, pad=0.5) {
  
  if (is.null(labels) || !length(labels))
    return(invisible())
  
  # w <- max(strwidth(labels,
  #                   units = "inches",
  #                   cex = par("cex.axis")))
  # 
  # lineHeight <- par("csi") * par("mex")
  # 
  # needed <- 1.1 * (ceiling(w / lineHeight) + 1)
  
  needed <- .neededMargin(labels, pad=pad)
  
  mar <- par("mar")
  
  if (needed > mar[2]) {
    mar[2] <- needed
    par(mar = mar)
  }
  
  invisible()
}



.adjustBottomMarginForLas2 <- function(labels) {
  
  if (is.null(labels) || !length(labels))
    return(invisible())
  
  w <- max(strwidth(labels,
                    units = "inches",
                    cex = par("cex.axis")))
  
  lineHeight <- par("csi") * par("mex")
  
  needed <- ceiling(w / lineHeight) + 1
  
  mar <- par("mar")
  
  if (needed > mar[1]) {
    mar[1] <- needed
    par(mar = mar)
  }
  
  invisible()
}





.neededMargin <- function(labels,
                          side = 2,
                          cex = par("cex.axis"),
                          pad = 0.5,
                          axis.line = 1) {
  
  if(length(labels) == 0)
    return(0)
  
  w <- max(strwidth(labels, units = "inches", cex = cex))
  h <- max(strheight(labels, units = "inches", cex = cex))
  
  size <- if(side %in% c(2,4)) w else h
  
  lineHeight <- par("csi") * par("mex")
  
  lines <- size / lineHeight
  
  ceiling(lines + axis.line + pad)
}




.outerAt <- function(at, side = 2) {
  
  usr <- par("usr")
  plt <- par("plt")
  
  if (side %in% c(2,4)) {
    # y direction
    plt[3] + (at - usr[3]) / diff(usr[3:4]) * diff(plt[3:4])
  } else {
    # x direction
    plt[1] + (at - usr[1]) / diff(usr[1:2]) * diff(plt[1:2])
  }
  
}




# options(DescToolsX.theme = list(
#   bg        = "white",
#   col       = "grey70",
#   border    = "white",
#   grid      = TRUE,
#   grid.col  = "grey85",
#   grid.lty  = 1,
#   grid.lwd  = 1
# ))


.getTheme <- function() {
  th <- getOption("DescToolsX.theme")
  if (is.null(th))
    th <- list()
  th
}



.theme <- function(...) {
  
  args <- list(...)
  th   <- .getTheme()
  
  out <- list()
  
  for (nm in names(args)) {
    
    arg <- args[[nm]]
    
    # theme entries belonging to this object
    idx <- startsWith(names(th), paste0(nm, "."))
    th_sub <- th[idx]
    
    # convert flat theme names -> property names
    if (length(th_sub)) {
      names(th_sub) <- sub(paste0("^", nm, "\\."), "", names(th_sub))
    }
    
    # special rename for pch.id -> pch
    if (nm == "pch" && "id" %in% names(th_sub)) {
      names(th_sub)[names(th_sub) == "id"] <- "pch"
    }
    
    # combine argument + theme
    if (is.null(arg)) {
      
      out[[nm]] <- th_sub
      
    } else if (is.list(arg)) {
      
      out[[nm]] <- modifyList(th_sub, arg)
      
    } else {
      
      # simple scalar (e.g. pch=16)
      if (nm == "pch")
        out[[nm]] <- modifyList(th_sub, list(pch = arg))
      else
        out[[nm]] <- modifyList(th_sub, list(col = arg))
      
    }
  }
  
  out
}




# 
# .theme <- function(...) {
#   
#   args <- list(...)
#   th   <- .getTheme()
#   
#   out <- vector("list", length(args))
#   names(out) <- names(args)
#   
#   for (nm in names(args)) {
#     out[[nm]] <- args[[nm]] %||% th[[nm]]
#   }
#   
#   out
# }
# 
# 




## == old stuff ============================================================

# # internal function to restore settings after a plot has been created
# 
# .withGraphicsState <- function(expr) {
#   
#   op <- par(no.readonly = TRUE)
#   ok <- FALSE
#   
#   on.exit({
#     # layout(matrix(1))
#     par(op)
#     
#     if (ok) {   ## && !is.null(.getOption("stamp"))) {
#       tryCatch(stamp(), error = function(e) NULL)
#     }
#     
#   }, add = TRUE)
#   
#   force(expr)
#   ok <- TRUE
#   
# }
# 


# base version
# # with layout saving option
# .withGraphicsState <- function(expr) {
#   
#   op <- par(no.readonly = TRUE)
#   opt <- options()
#   
#   hasLayout <- {
#     n <- layout.show(n = 0)
#     isTRUE(n > 0)
#   }
#   
#   ok <- FALSE
#   
#   on.exit({
#     par(op)
#     options(opt)
#     
#     if (hasLayout) {
#       ## layout wiederherstellen (falls ihr das implementiert habt)
#     }
#     
#     if (ok) {
#       tryCatch(stamp(), error = function(e) NULL)
#     }
#     
#   }, add = TRUE)
#   
#   # force(expr)
#   eval.parent(substitute(expr))
#   
#   ok <- TRUE
# }
# 
# 
# .withGraphicsState <- function(expr) {
#   
#   op <- par(no.readonly = TRUE)
#   ow <- options(warn = 1)
#   
#   ok <- FALSE
#   
#   on.exit({
#     if (ok) tryCatch(stamp(), error = function(e) NULL)
#     par(op)
#     options(ow)
#   }, add = FALSE)
#   
#   eval.parent(substitute(expr))
#   
#   ok <- TRUE
#   
#   invisible(NULL)
# }


# .withGraphicsState <- function(expr) {
# 
#   op <- par(no.readonly = TRUE)
#   withr::defer(par(op))
# 
#   withr::local_options(list(warn = 1))
# 
#   ok <- FALSE
# 
#   on.exit({
#     if (ok) tryCatch(stamp(), error = function(e) NULL)
#   }, add = TRUE)
# 
#   eval.parent(substitute(expr))
# 
#   ok <- TRUE
# 
#   invisible(NULL)
# }



# .withGraphicsState <- function(expr) {
#   
#   forbidden <- c(
#     "mfrow","mfcol","mfg",
#     "fig","plt","pin","fin",
#     "usr","new"
#   )
#   
#   before <- par(no.readonly = TRUE)
#   
#   op <- before[setdiff(names(before), forbidden)]
#   withr::defer(par(op))
#   
#   withr::local_options(warn = 1)
#   
#   stamp_opt <- .getOption("stamp")
#   
#   ok <- FALSE
#   
#   on.exit({
#     if (ok && !is.null(stamp_opt) && .isLastPanel())
#       tryCatch(stamp(), error = function(e) NULL)
#   }, add = TRUE)
#   
#   eval.parent(substitute(expr))
#   
#   after <- par(no.readonly = TRUE)
#   
#   changed <- forbidden[
#     vapply(forbidden, function(p)
#       !identical(before[[p]], after[[p]]), logical(1))
#   ]
#   
#   if (length(changed)) {
#     warning(
#       "The following 'par' parameters should not be modified inside this plotting function: ",
#       paste(changed, collapse = ", "),
#       call. = FALSE
#     )
#   }
#   
#   ok <- TRUE
#   
#   invisible(NULL)
# }
# 
