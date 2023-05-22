#' Launch a psych.pipeline instance from a single yaml file.
#'
#' This function launches the psych.pipeline either on a local machine or a high-performance computing cluster.
#'
#' @param yaml_path The path to the YAML file that contains the configuration settings for the pipeline. If NULL, the most recent YAML file in the current directory is used. Default is NULL.
#' @return The function does not return a value; it launches a single psych.pipeline call based on the parameters.
#'
#' @importFrom bannerCommenter section open_box
#' @importFrom pacman p_load
#' @importFrom devtools session_info
#'
#' @author Nate Hall
#' @export
#'

launch_psych.pipeline <- function(yaml_path = NULL) {#, pipeline_description = "psych.pipeline", run_on_hpc = FALSE, timestamp = TRUE) {

  # Identify the YAML configuration file to use
  if(is.null(yaml_path)){
    yaml_files <- list.files(".", pattern = ".yaml$", full.names = TRUE, recursive = TRUE)
    yaml_path <- Sys.getenv("YAML_PATH", yaml_files[which.max(file.mtime(yaml_files))])
  } else {
    yaml_path <- shQuote(yaml_path) # sanitize removing escape characters
  }

  # no yaml, no pipeline.
  stopifnot(file.exists(yaml_path))

  #configure the entire pipeline and store within config
  config <- configure_psych.pipeline(yaml_path)

  # load user's packages
  pacman::p_load(char = config$settings$packages)

  # Create log directory if it doesn't exist
  if (!dir.exists(config$path$log_full)) {
    dir.create(config$path$log_full, recursive = TRUE)
  }

  # create high-level output
  out_file <- file.path(config$path$log_full, "pipeline_execution_report.Rout")


  # Start diverting output
  sink(out_file)

  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                       STAMP WITH SESSION INFO                       ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################

  # Print configuration and session information
  print(bannerCommenter::section(paste0("settings and session info:\n\n", config$settings$project, " pipeline"), fold = FALSE))
  print(bannerCommenter::open_box("Bring User-defined Functions Into Scope"))

  # Source all .R files in the "R" directory
  source_directory("R")

  # Print pipeline configuration
  print(bannerCommenter::open_box("PipelineConfiguration"))
  list_tree(config)

  # Print session information
  print(devtools::session_info(pkgs = "attached"))

  ############################################################################
  ############################################################################
  ###                                                                      ###
  ###                           EXECUTE PIPELINE                           ###
  ###                                                                      ###
  ############################################################################
  ############################################################################

  if(config$settings$run_on_hpc){
    # implement if needed
  } else {
    cat("\n\n###########################################################################\n###########################################################################\n###                                                                     ###\n###                  RUNNING LOCALLY ON ", config$settings$n_cores, "CORES:                       ###\n###                                                                     ###\n###########################################################################\n###########################################################################\n\n\n")

    # Run pipeline!
    print("pipeline_go(config)")
    # pipeline_go(config)
  }

  cat("\n\n\n############################################################################\n############################################################################\n###                                                                      ###\n###                               COMPLETE                               ###\n###                                                                      ###\n############################################################################\n############################################################################\n")

  #end high-level output
  sink()

}



