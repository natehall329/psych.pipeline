#' Initialize Output Directories
#'
#' This function checks for the existence of output directories, and creates them if they do not exist. It is designed to handle both subject-level and non-subject level output directories.
#'
#' @param config A list containing configuration information. The `$path$output` element should contain the path to the output directory.
#' @param subj_control A data frame or tibble containing information about the control of subjects. The `subject_level` column should indicate whether the output for each step is subject-level, and the `data_destination` and `log_destination` columns should contain the output directory paths for each step.
#'
#' @return This function does not return a value, but it will create directories on the filesystem as a side effect.
#'
#' @examples
#' config <- list(path = list(output = "/path/to/output"))
#' subj_control <- data.frame(
#'   subject_level = c(FALSE, TRUE),
#'   data_destination = c("/path/to/data1", "/path/to/data2"),
#'   log_destination = c("/path/to/log1", "/path/to/log2")
#' )
#' init_outdirs(config, subj_control)
#'
#' @export
#'
#' @author Nate Hall

init_outdirs<- function(config, subj_control){

  ## since only subject-level outputs get their own nested directories just create these recursively. If nothing is subject level just create a single output dir
  if(!any(subj_control$subject_level)){
    if(!dir.exists(config$path$output)){
      dir.create(config$path$output)
    }
  } else{
    # some subject-level steps required
    s_levels <- subj_control %>% dplyr::filter(subject_level)
    for(i in 1:nrow(s_levels)){

      # subject-level outputs
      odir <- s_levels[i, ] %>% pull(data_destination) %>% unname()
      if(!dir.exists(odir)){
        dir.create(odir, recursive = TRUE)
      }

      # subject-level logs
      ldir <- s_levels[i, ] %>% pull(log_destination) %>% unname()
      if(!dir.exists(ldir)){
        dir.create(ldir, recursive = TRUE)
      }
    }
  }
}
