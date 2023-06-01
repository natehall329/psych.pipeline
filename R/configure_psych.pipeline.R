#' Configure Psychology Pipeline
#'
#' This function reads a YAML configuration file and sets default parameters for running a psychology pipeline.
#' The YAML file should contain sections for `path`, `settings`, and `pipeline`. Each section has its own default values
#' which will be used if not specified in the YAML file. Also, it extracts unique subject identifiers from a directory
#' or a .csv file depending on the configuration.
#'
#' @param yaml_path A string specifying the path to the YAML configuration file.
#'
#' @return A list. The modified YAML data after setting the default values and updating the pipeline.
#'
#' @examples
#' \dontrun{
#' # It is used to set up the pipeline configuration
#' configure_psych.pipeline(yaml_path = "./inst/dimt_params.yaml")
#' }
#'
#' @details
#' The function first checks if the YAML data has `path`, `settings`, and `pipeline` sections. If any section is not
#' present, it creates the section with default values. If a section is present but does not have all the fields, it
#' sets the missing fields to their default values.
#'
#' For the `path` section, the default values are `pipeline_fx = "R"`, `log = "outputs/pipeline_logfiles"`, and
#' `output = "data_preproc"`.
#'
#' For the `settings` section, the default values are `project = "untitled project"`,
#' `pipeline_description = "psych.pipeline"`, `timestamp = TRUE`, `n_cores = 1`, and `force = FALSE`.
#'
#' For each element in the `pipeline` section, the function sets default values for missing fields. The default values
#' are `depends_on = "none"`, `print_output = FALSE`, `describe_text = element$func`, `arguments = list()`,
#' `subject_level = FALSE`, and `force = default_settings$force`.
#'
#' @note
#' The function extracts unique subject identifiers by looking at filenames in a directory or by reading a .csv file.
#' It is important to ensure that the filenames or the .csv file contain the subject identifiers.
#'
#' @importFrom readr read_csv
#' @importFrom yaml yaml.load_file
#'
#' @seealso
#' \code{\link[yaml]{yaml.load_file}} for loading YAML files.
#' \code{\link[base]{file.info}}, \code{\link[base]{list.files}} for file and directory operations.
#' \code{\link[readr]{read_csv}} for reading .csv files.
#'
#' @author Nate Hall
#'
#' @export


configure_psych.pipeline <- function(yaml_path) {

  yaml_data <- yaml::yaml.load_file(yaml_path)

  # # Extract unique subject identifiers to be run. Run this first since without a value specified the whole pipeline will stop.
  # if (dir.exists(yaml_data$path$subject_list)) {
  #   # Get the list of files in the directory
  #   files <- list.files(yaml_data$path$subject_list, include.dirs = FALSE, all.files = FALSE)
  #
  #   # Filter out directories
  #   files <- files[!file.info(file.path(yaml_data$path$subject_list, files))$isdir]
  #
  #   # Remove the file extensions to get the subject IDs
  #   yaml_data$data$subject_list <- sub("\\..*", "", files)
  #
  # } else if (yaml_data$subject_list$type == 'directory') {
  #   # Load the .csv file and put the subject IDs into the global environment
  #   yaml_data$data$subject_list <- readr::read_csv(yaml_data$path$subject_list)
  # }

  # Set default values for path section
  default_path <- list(
    pipeline_fx = "R",
    log = "outputs/pipeline_logfiles",
    output = "data_preproc"
  )


  if (!"path" %in% names(yaml_data)) {
    yaml_data$path <- default_path
  } else {
    for (field in names(default_path)) {
      if (!field %in% names(yaml_data$path)) {
        yaml_data$path[[field]] <- default_path[[field]]
      }
    }
  }

  # Set default values for settings section
  default_settings <- list(
    project = "untitled project",
    pipeline_description = "psych.pipeline",
    timestamp = TRUE,
    n_cores = 1,
    force = FALSE
   # packages
  )

  if (!"settings" %in% names(yaml_data)) {
    yaml_data$settings <- default_settings
  } else {
    for (field in names(default_settings)) {
      if (!field %in% names(yaml_data$settings)) {
        yaml_data$settings[[field]] <- default_settings[[field]]
      }
    }
  }

  # Append timestamp if requested
  if (yaml_data$settings$timestamp) {
    timestamp_string <- format(Sys.time(), "%Y%m%d-%H%M%S")
    yaml_data$settings$pipeline_description <- paste0(yaml_data$settings$pipeline_description, "_", timestamp_string)
  }

  ##### compute unique path to submission dir using combination of yaml (generic location) and submission ID (tagged with specific date/time stamp)
  yaml_data$path$log_full <- file.path(yaml_data$path$log, yaml_data$settings$pipeline_description)


  # Set default values for pipeline elements
  pipeline <- yaml_data$pipeline

  for (e_num in 1:length(pipeline)) {

    element <- pipeline[[e_num]]
    # Set default values for missing fields
    if (!"depends_on" %in% names(element)) {
      element$depends_on <- "none"
    }
    if (!"print_output" %in% names(element)) {
      element$print_output <- FALSE
    }
    if (!"describe_text" %in% names(element)) {
      element$describe_text <- element$func
    }
    if (!"arguments" %in% names(element)) {
      element$arguments <- list()
    }
    if (!"subject_level" %in% names(element)) {
      element$subject_level <- FALSE
    }
    if (!"force" %in% names(element)) {
      element$force <- default_settings$force
    }

    # Loop over each argument of the pipeline step
    for (arg in names(element$arguments)) {
      # Check if the argument's value is a reference to a top-level part
      if (grepl("\\$", element$arguments[[arg]])) {
        # Split the reference into the top-level part's name and the key
        parts <- strsplit(element$arguments[[arg]], "\\$")[[1]]
        top_level_name <- parts[1]
        key <- parts[2]
        # Replace the reference with the actual value
        element$arguments[[arg]] <- yaml_data[[top_level_name]][[key]]
      }
    }

    # re-assign
    pipeline[[e_num]] <- element
  }

  names(pipeline) <- sapply(pipeline, function(x) x$func)


  # Update the modified pipeline in the YAML data
  yaml_data$pipeline <- pipeline

  return(yaml_data)
}
