#' Execute R Code with Error/Warning Logging
#'
#' This function executes an R expression or block of code, logging errors and warnings.
#' It optionally logs output to a specified file and stores the output in a RDS file.
#'
#' @param code An R expression or block of code to be executed. Typically a function of an analytic pipeline.
#' @param log_path Character string. Path to the log file where errors and warnings are to be written.
#' If NULL (default), output is written to the console.
#' @param output_path Character string. Path to the RDS file where the output of the code is to be stored.
#' If NULL (default), the output is not stored.
#' @param describe_text Character string. A description text to include in the log output.
#' If NULL (default), no description text is added.
#'
#' @return If there is an error or warning during code execution, a `try-error` or `simpleWarning` object is returned,
#' respectively. If the code executes successfully, the output of the code is returned.
#'
#' @examples
#' \dontrun{
#'   execute_log({print("Hello, world!")}, log_path = "my_log.txt")
#' }
#'
#' @author Nate Hall
#'
#' @export

execute_log <- function(code, log_path = NULL, output_path = NULL, describe_text = NULL) {
  if (!is.null(log_path)) {
    log_conn <- file(log_path, open = "wt")
    sink(log_conn)
    sink(log_conn, type = "message")
  }

  o <- tryCatch(code, error = function(c) {
    if (is.null(describe_text)) {
      cat("ERROR (", gsub("Error: ", "", gsub("\\n", "", as.character(c))), ")\n", sep = "")
    } else {
      cat(describe_text, " ERROR (", gsub("Error: ", "", gsub("\\n", "", as.character(c))), ")\n", sep = "")
    }
    return(c)
  }, warning = function(c) {
    if (is.null(describe_text)) {
      cat("WARNING (", gsub("simpleWarning: ", "", gsub("\\n", "", as.character(c))), ")\n", sep = "")
    } else {
      cat(describe_text, " WARNING (", gsub("simpleWarning: ", "", gsub("\\n", "", as.character(c))), ")\n", sep = "")
    }
    return(c)
  })

  if (!any(c("error", "warning") %in% class(o)) && !is.null(describe_text)) {
    cat("\n", describe_text, ": SUCCESS\n", sep = "")
  }

  if (!is.null(output_path)) {
    output_objects <- list(
      func_out = o,
      execution_log = log_path)
    saveRDS(output_objects, file = output_path)
    cat("\nFunction output saved in: ", output_path, "\n")
  } else {
    output_objects <- o
  }

  if (!is.null(log_path)) {
    sink()
    sink(type = "message")
    close(log_conn)
  }

  return(output_objects)
}
