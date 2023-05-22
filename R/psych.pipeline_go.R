#' @title Execute a Configurable Pipeline via psych.pipeline
#'
#' @description This function executes a set of functions in a pipeline based on the configuration provided. It checks for dependencies, manages
#' multi-output and single-output steps, and enforces re-run of specific steps if specified. It also logs the execution process and prints outputs if required.
#'
#' @param config A list containing configuration for the pipeline. It includes:
#' * `pipeline`: A list of elements. Each element is a list including:
#'   * `func`: the function to be executed in the pipeline step.
#'   * `arguments`: the arguments to be passed to the function.
#'   * `depends_on`: dependencies of the function. It should be "none" if there are no dependencies.
#'   * `force`: a boolean value indicating whether to re-run the function regardless of whether the output exists.
#'   * `multiple_outputs`: a boolean indicating whether the function will output multiple files.
#'   * `print_output`: a boolean indicating whether to print the output of the function.
#' * `path`: A list containing:
#'   * `log_full`: the full path to store log files.
#'   * `output`: the path to store output files.
#'
#' @return Nothing is returned by this function. However, it executes the steps in the pipeline based on the configuration,
#' creating log files and output files in the specified paths.
#'
#' @seealso `execute_log()`
#'
#' @examples
#' \dontrun{
#'   # Define configuration for the pipeline
#'   config <- list(
#'     pipeline = list(
#'       list(
#'         func = "myFunc1",
#'         arguments = list(arg1 = 1, arg2 = 2),
#'         depends_on = "none",
#'         force = TRUE,
#'         multiple_outputs = TRUE,
#'         print_output = FALSE
#'       ),
#'       list(
#'         func = "myFunc2",
#'         arguments = list(arg1 = 3, arg2 = 4),
#'         depends_on = "myFunc1",
#'         force = FALSE,
#'         multiple_outputs = FALSE,
#'         print_output = TRUE
#'       )
#'     ),
#'     path = list(
#'       log_full = "/path/to/log",
#'       output = "/path/to/output"
#'     )
#'   )
#'
#'   # Execute the pipeline
#'   pipeline_go(config)
#' }
#'
#' @author Nate Hall
#' @export


psych.pipeline_go <- function(config) {
  # Create a list to store the subjects that need to be re-run for each step
  force_subjects <- vector("list", length(config$pipeline))
  names(force_subjects) <- sapply(config$pipeline, function(x) x$func)

  # For each element in the pipeline...
  for (i in seq_along(config$pipeline)) {
    step <- config$pipeline[[i]]

    # Get the function to be run and its arguments
    func <- get(step$func)
    args <- step$arguments

    # If there are dependencies, check that they have run.
    # This involves checking the existence of output files. and loading them into environment as arguments
    if (step$depends_on != "none") {

      # only allow dependencies based on previous steps
      available_dependencies <- sapply(config$pipeline[1:i-1], function(f) f$func)
      # requested dependencies @ this step
      requested_dependencies <- args[which(grepl("^depends_on\\[\\d+\\]$",args))]


      for (dep_num in seq_along(requested_dependencies)) {

        dependency <- step$depends_on[dep_num]

        #dependency requested needs to be avail
        stopifnot(all(dependency %in% available_dependencies))

        # gather information on this dependency
        matching_element <- execute_log(config$pipeline[[which(unlist(lapply(config$pipeline, function(x) x$func)) == dependency)]], describe_text = paste0("Searching for dependency ", dep_num , " (", dependency, ")"))

        # dependency_path <- file.path(config$path$output, dependency)
        # If the dependency has multiple outputs, check for each subject file
        if (matching_element$multiple_outputs) {
          # step$arguments <-
        } else {
          # dependency is a single .rds file. simple.
          step$arguments[which(step$arguments == paste0("depends_on[", dep_num,"]"))] <- readRDS(file.path(config$path$output, ))
          # args[[dependency]]
        }
      }


    }


    # If force is a list of subjects, add them to the force_subjects for this step
    if (is.list(step$force)) {
      if(step$multiple_outputs){
        force_subjects[[step$func]] <- step$force
      } else {
        # unless it's a single output step.
        cat("Step: ", step$func, " is a single output process. Therefore, specifying individual subjects to force in this step will be ignored.")
      }
    }

    if(step$multiple_outputs){
      # Get the subjects to be run for this step
      subjects_to_run <- force_subjects[[step$func]]
      # Check if output already exists
      existing_subjects <- list.files(output_path)
      output_exists <- setdiff(subjects, existing_subjects)
    } else {
      ### Single output version
      # Construct the paths for the log and output files for this step.
      log_path <- file.path(config$path$log_full, paste0(step$func, ".log"))
      output_path <- file.path(config$path$output, paste0(step$func, ".rds"))
      # Check if output already exists
      output_exists <- file.exists(output_path)

      # If force is TRUE, or if output does not exist, run the function
      if (step$force == TRUE || !output_exists) {
        # one day we can opt for keeping objects stored in memory, though i can see this being problematic for large pipelines.
        o <- execute_log(do.call(func, step$arguments), log_path, output_path, step$describe_text)

        #
        if(step$print_output){
          # Save the current width option value
          original_width <- getOption("width")
          # Set the width to a larger value so you can scroll and lookup notes. this renders well when softwrap is toggled to false.
          options(width = 10000)
          print(o, right = FALSE,  width = Inf)
          # Reset the width option to its original value
          options(width = original_width)
        }
      }

    }
  }
}

