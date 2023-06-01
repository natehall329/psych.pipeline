#' track_dependencies
#'
#' This function is used to recursively track dependencies and return subjects
#' that need to be forced to re-run based on the provided pipeline configuration
#' and the control object for subject runs.
#'
#' @description
#' \code{track_dependencies} is an internal function not intended to be used
#' directly by users. It's part of the dependency tracking subsystem of the
#' pipeline management framework.
#'
#' @param step_index Integer. The index of the current step in the pipeline configuration.
#' @param config List. The configuration object for the pipeline. It should contain
#' a component named 'pipeline', which is a list of steps. Each step should be a list
#' with at least a 'depends_on' component which is either "none" or a vector
#' of names of other steps this step depends on.
#' @param subj_control List. The control object for subject runs. It should
#' contain a component named 'run_subjects' which is a list of vectors, each vector
#' is a list of subject identifiers that are set to run for each step.
#'
#' @return A list of unique subject identifiers which are needed to be forced
#' to re-run due to the dependencies of the current step.
#'
#' @keywords internal
#'
#' @author Nate Hall

track_dependencies <- function(step_index, config, subj_control) {
  # Recursive function to track dependencies and return subjects to be forced to re-run

  current_step <- config$pipeline[[step_index]]

  if (current_step$depends_on == "none") {
    return(subj_control$run_subjects[[step_index]])
  } else {
    run_subjects <- list()
    for (dep in current_step$depends_on) {
      dep_index <- match(dep, names(config$pipeline)) # find index of dependent step
      run_subjects <- c(run_subjects, track_dependencies(dep_index, config, subj_control)) # recursively track dependencies
    }
    return(unique(run_subjects)) # ensure no duplicate subjects
  }
}
