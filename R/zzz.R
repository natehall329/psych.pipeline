#' Internal function to set package options
#'
#' This function is run when the package is loaded to set various package options.
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Disable ANSI escape characters in outputs. Helps alot with pure text outputs but we can mess with this at another time.
  options(crayon.enabled = FALSE)
}

