#' Set Up Subject Control
#'
#' This function sets up control over which subjects to process through each step of a pipeline.
#' It takes into account the dependencies between different steps.
#'
#' @param config A list. It should contain the pipeline configuration, including each step's function name, its dependencies, whether the step operates on the subject level, the subjects to force run for this step, and so on.
#'
#' @return A tibble, `subj_control`, which contains the control information for each step. Columns:
#' - step: The step number in the pipeline.
#' - func: The function name for this step.
#' - run_subjects: A list of subject ids that should be processed in this step. This list takes into account forced subjects and step dependencies.
#' - data_destination: The output path for this step.
#'
#' @details
#' This function first extracts the master list of subject ids from the directory or CSV file specified in `config$path$subject_list`.
#' Then it initializes a tibble, `subj_control`, to control which subjects to process through each step.
#' The initial list of subjects to run for each step is set to all subjects.
#' Then, for each step, it checks if the step has any dependencies.
#' If so, it finds the subjects that were forced to run for the dependent steps, and ensures these subjects will also run for the current step.
#' Finally, it handles the `force` option for each step: if `force` is a list of subjects, these subjects are added to `run_subjects` for this step (if the step operates on the subject level);
#' if `force` is a logical value, it determines whether to re-run all subjects or only the ones that don't exist in the destination folder;
#' if `force` is neither a list nor a logical value, it defaults to running all subjects for this step.
#'
#' @note
#' Ensure that `track_dependencies()` function is defined in the same environment as `setup_subj_control()`.
#' Make sure to test this thoroughly with your specific case, as recursion and dependencies can get complex and might have edge cases or specific behaviors that need to be addressed.
#'
#' @author Nate Hall
#' @export

setup_subj_control <- function(config) {

  # extract master list of subject ids from directory or csv
  subjects <- get_subj_ids(config$path$subject_list)

  # initialize a data frame to be used later for determining which subjects to run
  subj_control <- tibble(step = 1:length(config$pipeline),
                         func = names(config$pipeline),
                         run_subjects = list(subjects),
                         data_destination = ifelse(sapply(config$pipeline, function(step){step$subject_level}),  file.path(config$path$output, names(config$pipeline)), file.path(config$path$output, paste0(names(config$pipeline), ".rds"))))

  for(st in 1:nrow(subj_control)){
    step <- config$pipeline[[st]]

    # Instead of directly checking 'force', use track_dependencies to consider dependencies
    forced_subjects <- track_dependencies(st, config, subj_control)
    subj_control$run_subjects[[st]] <- forced_subjects

    # Extract subjects to be forced during subject-level steps
    if (is.list(step$force)) {
      if(step$subject_level){
        # If force is a list of subjects, add them to run_subjects for this step
        subj_control[st, "run_subjects"] <- list(intersect(subj_control$run_subjects[[st]], step$force)) # in this case they are specific subjects
      } else {
        # unless it's a single output step.
        cat("Step: ", step$func, " is a single output process. Therefore, specifying individual subjects to force in this step will be ignored.")
        subj_control[st, "run_subjects"] <- NA
      }
    } else if (is.logical(step$force)) {
      # users can also pass TRUE/FALSE to force (say, if they have to debug their code) and then this forces the re-running of all subjects.
      if(!step$force){
        # since we initialize df at running all subjects at all steps, all we need to do at this point is to run all subjects that dont exist in the destination folder
        completed <- list.files(subj_control[st, ] %>% pull(data_destination))
        subj_control$run_subjects[st] <- list(subjects[which(!subjects %in% completed)])
      }
    } else {
      cat("Step:", step$func, "does not list subjects to force run and has not set default to T/F - performing this step on all subjects!")
    }
  }

  return(subj_control)

}
