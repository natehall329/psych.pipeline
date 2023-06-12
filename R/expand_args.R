#' Expand Arguments Based on Run Subjects
#'
#' This function takes a list of arguments and a list of subjects to run,
#' and expands the arguments into a list of argument lists, one for each subject.
#' The `subj_dat` argument is replaced with the full path to the subject's data file.
#'
#' @param args A named list of arguments. The list must include a `subj_dat` element
#'   which is the path to the directory containing the data files.
#' @param run_subjects A character vector of subject IDs to include.
#'   These IDs should match the base names of the data files (excluding the file extension).
#'
#' @return A list of argument lists, one for each subject in `run_subjects`.
#'   Each argument list includes the full path to the subject's data file in the `subj_dat` argument,
#'   and all other arguments are copied from `args`.
#'
#' @examples
#' args <- list(subj_dat = "data_raw/behav/dimt", arg2 = 2, arg3 = 3)
#' run_subjects <- c("137", "169", "246", "248", "255", "272", "317", "363", "364", "365", "386", "423", "434", "452", "482", "92")
#' args_list <- expand_args(args, run_subjects)
#'
#' @author Nate Hall
#'
#' @export

expand_args <- function(args, run_subjects) {
  library(tidyverse)

  # Get the names of all the files in the subj_dat directory
  file_names <- list.files(path = args$subj_dat, full.names = TRUE)

  # Remove the file extensions and get basename
  file_names_no_ext <- file_names %>%
    str_remove("\\..*$") %>%
    basename()

  # Get the file extensions
  file_extensions <- str_extract(file_names, "\\..*$")

  # Match file names with run_subjects
  match_indices <- match(run_subjects, file_names_no_ext)

  # Filter out NA values (no match found)
  match_indices <- match_indices[!is.na(match_indices)]

  # Create list of arguments for each matched subject
  args_list <- lapply(match_indices, function(index) {
    # Create a copy of args
    args_copy <- args

    # Replace subj_dat with the full path to the subject's data file, adding the file extension back on
    args_copy$subj_dat = file.path(args$subj_dat, paste0(file_names_no_ext[index], file_extensions[index]))

    # Return the modified args
    args_copy
  })

  return(args_list)
}
