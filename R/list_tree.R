#' Print a tree-like structure of a list
#'
#' This function prints a tree-like structure of a list to the console. It handles nested lists,
#' and uses special characters to represent the branching of the tree.
#'
#' @param x A list to print. This list can be nested (i.e., contain other lists).
#' @param prefix A string used to control the indentation of the tree structure.
#' @param is_tail A logical value indicating whether the current item is the last in its parent list.
#'
#' @return Invisibly returns the original list, `x`, and primarily operates through side effects (printing to the console).
#' @examples
#' \dontrun{
#' x <- list(
#'   DESCRIPTION = "Package description",
#'   NAMESPACE = "Package namespace",
#'   R = list(
#'     function1 = "Code for function 1",
#'     function2 = "Code for function 2"
#'   )
#' )
#' list_tree(x)
#'
#' }
#'
#'
#' @export

list_tree <- function(x, prefix = "", is_tail = TRUE) {
  if (is.list(x)) {
    n <- length(x)
    names(x) <- ifelse(is.na(names(x)), seq_len(n), names(x))
    for (i in seq_len(n)) {
      cat(prefix, ifelse(i < n, "\u251C\u2500\u2500 ", "\u2514\u2500\u2500 "), names(x)[i], "\n", sep = "")
      new_prefix <- paste0(prefix, ifelse(i < n, "\u2502   ", "    "))
      list_tree(x[[i]], new_prefix, i == n)
    }
  } else {
    cat(prefix, ifelse(is_tail, "\u2514\u2500\u2500 ", "\u251C\u2500\u2500 "), x, "\n")
  }
  invisible(x)
}

