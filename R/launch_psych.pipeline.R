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
  print(bannerCommenter::section(paste0("psych.pipeline functions and session info\n\n\n project: ", config$settings$project, "\n\npipeline: ", config$settings$pipeline_description), fold = FALSE))
  print(bannerCommenter::open_box("Bring User-defined Functions Into Scope"))

  # Source all .R files in the "R" directory
  source_directory("R")

  # Print session information
  print(bannerCommenter::open_box("Session Info"))
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

    # Print pipeline configuration
    print(bannerCommenter::section(paste0("final configuration details for:\n\n", config$settings$pipeline_description)))
    print(open_box("High-level Pipeline Settings"))
    list_tree(config$settings)
    print(open_box("Paths to Data and Output Locations"))
    list_tree(config$path)
    print(open_box("Step-by-Step Execution of Pipeline Elements"))
    list_tree(config$pipeline)




    # Run pipeline!
    print(bannerCommenter::section("running psych.pipeline locally on", config$settings$n_cores, "cores"))
    print("pipeline_go(config)")
    # psych.pipeline_go(config)
  }

  cat("\n\n\n############################################################################\n############################################################################\n###                                                                      ###\n###                               COMPLETE                               ###\n###                                                                      ###\n############################################################################\n############################################################################\n")

  #end high-level output
  sink()

}


##---------------------
##  NTH scratch below
##---------------------

# yaml_path = NULL
# source("~/r_packages/psych.pipeline/R/list_tree.R")
# source("~/r_packages/psych.pipeline/R/execute_log.R")
# source("~/r_packages/psych.pipeline/R/source_directory.R")
# source("~/r_packages/psych.pipeline/R/configure_psych.pipeline.R")
# setwd("~/Documents/github_repos/arl_repos/dimt_analysis/")

