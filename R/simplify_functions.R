#' simplify_functions
#'
#' This function is a combination of the simplify function of the package deriv and a method to include undefined numerics in a function and still be simplify the function correctly. The undefined numerics should be given in the function as "numeric". They will not be handled as 1 variable, but as many differnt ones. Therefore they will only be simplifyed in the two cases.
#'In case of multiple additions/substracion of only a numeric, e.g. +numeric+numeric-numeric., the will be summarazied into one numeric: +numeric-. In case of  more than one multiplication/substraction they will also be summarized, e.g. numeric*numeric/numeric -> numeric
#' @param funs Character vector of functions to simplify.
#' @param function_variables Variables that are present in the functions. Function generated without any of the given variables will be returned as NAs.
#' @param no_cores Number of cores to use. If not defined, detectCores()-2 is used.
#'
#' @return A character vector with simplified functions
#' @export
#'
#' @examples
simplify_functions <- function(function_strings, function_variables, no_cores = NULL){
# Calculate the number of cores
  if(is.null(no_cores)){
    no_cores <- parallel::detectCores() - 2
  }
  # Initiate cluster
  start <- Sys.time()
  functions_list <- parallel::mclapply(function_strings,
                                       .simplify_single_function,
                                       function_variables = function_variables,
                                       mc.cores = no_cores)
  end <- Sys.time()
  cat("\nParallel simplify took ", (as.numeric(end) - as.numeric(start))/60, "minutes\n")
  return(unlist(functions_list))
}






