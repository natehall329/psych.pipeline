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

  # # debug
  yaml_path = NULL
  setwd("~/Documents/github_repos/arl_repos/dimt_analysis/")
  library(psych.pipeline)

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

  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                       STAMP WITH SESSION INFO                       ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################

  # create high-level output
  settings_log <- file.path(config$path$log_full, "psych.pipeline_settings.log")
  execution_log <- file.path(config$path$log_full, "psych.pipeline_execution.log")


  # Start diverting output
  sink(settings_log)

  # Print configuration and session information
  print(bannerCommenter::section(paste0("psych.pipeline functions and session info\n\n project: ", config$settings$project, "\npipeline execution label: ", config$settings$pipeline_description, "\n\nSEE:\n\n", execution_log, "\n\nand supporting execution logs for detailed progress reports"), fold = FALSE))
  print(bannerCommenter::open_box("Bring User-defined Functions Into Scope"))

  # Source all .R files in the "R" directory
  source_directory("R")

  # Print session information
  print(bannerCommenter::open_box("Session Info"))
  print(devtools::session_info(pkgs = "attached"))

  # Print pipeline configuration
  print(bannerCommenter::section(paste0("final configuration details for:\n\n", config$settings$pipeline_description)))
  print(open_box("High-level Pipeline Settings"))
  list_tree(config$settings)
  print(open_box("Paths to Data and Output Locations"))
  list_tree(config$path)

  # Go step-by step to give detailed info on each step of the pipeline
  print(section("Step-by-Step Pipeline Config Settings"))

  for(i in 1:length(config$pipeline)){
    print(open_box(paste0(i, ": ", config$pipeline[[i]]$func)))
    list_tree(config$pipeline[[i]])
  }

  sink()

  ############################################################################
  ############################################################################
  ###                                                                      ###
  ###                           EXECUTE PIPELINE                           ###
  ###                                                                      ###
  ############################################################################
  ############################################################################

  # Start diverting output
  sink(execution_log)

  if(config$settings$run_on_hpc){
    # implement using rslurm::slurm_apply
  } else {

    # Run pipeline!
    print(bannerCommenter::section(paste0("running psych.pipeline locally on ", config$settings$n_cores, " cores\n\n"), "see:\n", settings_log, "\nfor detailed information about the pipeline configuration", fold = FALSE))

    print("pipeline_go(config)")
    # psych.pipeline_go(config)
  }

  print(bannerCommenter::section("psych.pipeline execution complete", fold = FALSE))

  #end high-level output
  sink()

}

