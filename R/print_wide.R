#' Print a tibble without restricting width
#'
#' This function prints a tibble with all columns visible.
#' @param tib A tibble to be printed.
#' @examples
#' # Assuming df is a tibble
#' print_wide(df)
#'
#' @export

print_wide <- function(tib) {

  # Save the current width option value
  original_width <- getOption("width")
  # Set the width to a larger value so you can scroll and lookup notes. this renders well when softwrap is toggled to false.
  options(width = 10000)
  print(tib, right = FALSE)
  # Reset the width option to its original value
  options(width = original_width)

}
