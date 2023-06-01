#' Get Subject IDs
#'
#' This function extracts the subject IDs from a specified directory or a .csv file.
#' If a directory is provided, the function will return a list of file basenames (without extensions) present in the directory.
#' If a .csv file is provided, the function will read the file and return the first column as a vector.
#'
#' @param subject_list A character string specifying the path to a directory or a .csv file.
#'
#' @return
#' If subject_list is a directory, returns a character vector of file basenames present in the directory without their extensions.
#' If subject_list is a .csv file, returns a character vector of the contents of the first column in the .csv file.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_subj_ids("/path/to/directory")
#' get_subj_ids("/path/to/file.csv")
#' }
#'
#' @note
#' If the subject_list input is neither a .csv file nor a directory, the function will stop and throw an error.
#'
#' @seealso \code{\link{dir.exists}}, \code{\link{file.exists}}, \code{\link{read.csv}}
#'
#' @author Nate Hall

get_subj_ids <- function(subject_list) {
  # Check if subject_list is a directory
  if (dir.exists(subject_list)) {
    # Get all filenames in the directory, without extensions
    all_files <- list.files(subject_list, full.names = TRUE)

    # Filter out directories
    files <- all_files[!file.info(all_files)$isdir]

    # Get basenames without extensions
    file_basenames <- tools::file_path_sans_ext(basename(files))
    return(file_basenames)
  }
  # Check if subject_list is a file
  else if (file.exists(subject_list)) {
    # Check if it is a CSV file
    if (tools::file_ext(subject_list) == "csv") {
      # Read the CSV file and extract the first column as a vector
      subject_vector <- read.csv(subject_list, stringsAsFactors = FALSE)[, 1]
      return(subject_vector)
    } else {
      stop("Unknown subject_list input: needs to be a .csv file or a directory.")
    }
  }
  # If subject_list is neither a file nor a directory
  else {
    stop("Unknown subject_list input: needs to be a .csv file or a directory.")
  }
}
