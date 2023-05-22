#' Source all R scripts in a directory
#'
#' This function sources all the R scripts in a given directory by calling the
#' source() function on each file with a ".R" extension. This function can be
#' used to easily source all the scripts in a directory.
#'
#' @param dir_path A character string specifying the path to the directory
#'   containing the R scripts to be sourced.
#' @param recursive list recurisvely into the directory being sourced
#'
#' @return This function has no return value.
#'
#' @examples
#' \dontrun{
#' # Source all R scripts in the current working directory
#' source_directory(getwd())
#' }
#'
#' @author Nate Hall
#'
#' @export

source_directory <- function(dir_path, recursive = TRUE) {
  # Obtain a list of all .R files in the directory
  r_files <- list.files(dir_path, pattern = "\\.R$", full.names = TRUE, recursive = recursive)

  # Check if there are any .R files
  if (length(r_files) > 0) {
    # Source each file
    for (file in r_files) {
      cat("Sourcing:", file, "\n")
      source(file)
    }
    cat("Sourced all .R files in the directory:", dir_path, "\n")
  } else {
    cat("No .R files found in the directory:", dir_path, "\n")
  }
}
