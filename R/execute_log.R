#' Execute a block of code and log the output
#'
#' This function takes a block of code, executes it, and logs the output, along with any warnings or errors.
#' It can optionally also measure the runtime of the code block and save the output to a file.
#'
#' @param code The code block to execute. This should be a function or other executable R object.
#' @param log_path (Optional) A character string specifying the path to the file where the log should be written.
#'                If NULL (the default), no log file is created.
#' @param output_path (Optional) A character string specifying the path to the file where the output should be saved.
#'                   If NULL (the default), the output is not saved to a file.
#' @param describe_text (Optional) A character string that will be printed before the code execution.
#'                     It's typically used to describe the purpose or function of the code block.
#'                     If NULL (the default), no descriptive text is printed.
#' @param display_runtime (Optional) A logical value indicating whether the runtime of the code block should be printed.
#'                       If TRUE (the default), the runtime is printed.
#'
#' @return The output of the executed code block. If `output_path` is provided, the output is also saved to the specified file.
#'
#' @examples
#' \dontrun{
#' execute_log({print("Hello, World!")}, log_path = "log.txt", output_path = "output.txt",
#'             describe_text = "Printing a greeting", display_runtime = TRUE)
#' }
#'
#' @export

execute_log <- function(code, log_path = NULL, output_path = NULL, describe_text = NULL, display_runtime = TRUE) {

  if(display_runtime) {tictoc::tic("Run time")}

  if (!is.null(log_path)) {
    log_conn <- file(log_path, open = "wt")
    sink(log_conn)
    sink(log_conn, type = "message")
    print(bannerCommenter::boxup("Begin Execution"))
  }

  o <- withCallingHandlers({
    result <- code
    result
  }, error = function(c) {
    if (is.null(describe_text)) {
      cat("ERROR (", gsub("Error: ", "", gsub("\\n", "", as.character(c$message))), ")\n", sep = "")
    } else {
      cat(describe_text, " ERROR (", gsub("Error: ", "", gsub("\\n", "", as.character(c$message))), ")\n", sep = "")
    }
  }, warning = function(c) {
    if (is.null(describe_text)) {
      cat("WARNING (", gsub("simpleWarning: ", "", gsub("\\n", "", as.character(c$message))), ")\n", sep = "")
    } else {
      cat(describe_text, " WARNING (", gsub("simpleWarning: ", "", gsub("\\n", "", as.character(c$message))), ")\n", sep = "")
    }
  })

  print(bannerCommenter::boxup("Execution Output and Messages"))

  if (!any(c("error", "warning") %in% class(o)) && !is.null(describe_text)) {
    cat("\n", describe_text, ": DONE\n", sep = "")
  }

  print(bannerCommenter::boxup("Logging and Cleanup"))

  if (!is.null(output_path)) {
    output_objects <- list(
      func_out = o,
      execution_log = log_path)
    saveRDS(output_objects, file = output_path)
    cat("\nFunction output saved in: ", output_path, "\n")
  } else {
    output_objects <- o
  }

  if(display_runtime) {tictoc::toc()}

  if (!is.null(log_path)) {
    sink()
    sink(type = "message")
    close(log_conn)
  }

  return(output_objects)
}
