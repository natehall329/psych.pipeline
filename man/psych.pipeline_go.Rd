% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/psych.pipeline_go.R
\name{psych.pipeline_go}
\alias{psych.pipeline_go}
\title{Execute a Configurable Pipeline via psych.pipeline}
\usage{
psych.pipeline_go(config)
}
\arguments{
\item{config}{A list containing configuration for the pipeline. It includes:
\itemize{
\item \code{pipeline}: A list of elements. Each element is a list including:
\itemize{
\item \code{func}: the function to be executed in the pipeline step.
\item \code{arguments}: the arguments to be passed to the function.
\item \code{depends_on}: dependencies of the function. It should be "none" if there are no dependencies.
\item \code{force}: a boolean value indicating whether to re-run the function regardless of whether the output exists.
\item \code{subject_level}: a boolean indicating whether the function will output multiple files.
\item \code{print_output}: a boolean indicating whether to print the output of the function.
}
\item \code{path}: A list containing:
\itemize{
\item \code{log_full}: the full path to store log files.
\item \code{output}: the path to store output files.
}
}}
}
\value{
Nothing is returned by this function. However, it executes the steps in the pipeline based on the configuration,
creating log files and output files in the specified paths.
}
\description{
This function executes a set of functions in a pipeline based on the configuration provided. It checks for dependencies, manages
multi-output and single-output steps, and enforces re-run of specific steps if specified. It also logs the execution process and prints outputs if required.
}
\examples{
\dontrun{
  # Define configuration for the pipeline
  config <- list(
    pipeline = list(
      list(
        func = "myFunc1",
        arguments = list(arg1 = 1, arg2 = 2),
        depends_on = "none",
        force = TRUE,
        subject_level = TRUE,
        print_output = FALSE
      ),
      list(
        func = "myFunc2",
        arguments = list(arg1 = 3, arg2 = 4),
        depends_on = "myFunc1",
        force = FALSE,
        subject_level = FALSE,
        print_output = TRUE
      )
    ),
    path = list(
      log_full = "/path/to/log",
      output = "/path/to/output"
    )
  )

  # Execute the pipeline
  pipeline_go(config)
}

}
\seealso{
\code{execute_log()}
}
\author{
Nate Hall
}
