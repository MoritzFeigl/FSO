#' grammar
#'
#' @return Grammar object
#' @export
#'
#' @examples
#' grammar(a = "<b> + 1, <c>(<b>)",
#'         b = "1 + <c>(3), 2",
#'         c = "log, exp, max")
create_grammar <- function(...) {
  def <- list(...)
  # rename non-terminals
  names(def) <- paste0("<", names(def), ">")
  # apply rule function
  def_gram <- lapply(def, .rule)
  def_gram <- lapply(def_gram, function(x) gsub("\n", "", x))
  return(def_gram)
}

