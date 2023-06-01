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
    # Find the length of the longest filename
    max_length <- max(nchar(basename(r_files)))

    # Generate a comment line of this length
    comment_line <- paste0("##", strrep("—", max_length))

    # Print the initial comment line
    cat(comment_line, "\n")

    # Source each file
    current_dir <- dirname(r_files[1])  # Initialize current_dir to the directory of the first file
    for (file in r_files) {
      new_dir <- dirname(file)
      if (new_dir != current_dir) {
        cat(comment_line, "\n")  # Print a comment line when switching directories
        current_dir <- new_dir
      }
      cat("Sourcing:", file, "\n")
      source(file)
    }
    cat(comment_line, "\nSourced all .R files in the directory:", dir_path, "\n")
  } else {
    cat(comment_line, "\nNo .R files found in the directory:", dir_path, "\n")
  }
}
