#' variable_input
#'
#' @param functions A character vector or data frame (ncol = 1) of function strings.
#' @param variables A character vector with variable names that should be sampled from.
#' @param numbers A numeric vector with numbers that should be sampled from
#' @param n_iter Number of samples produced for a single functions.
#' @param no_cores Integer Number of cores to be used for computation, or NULL to set number of cores to parallel::detectCores()-2 or 1
#' @param var_string Character used for variables in functions.
#' @param num_string Character used for numebrs in functions.
#' @param seed An integer to be supplied to set.seed, or NULL not to set reproducible seeds.
#' @param save TRUE/FALSE, if a .feather of the output should be saved in the current working directory.
#'
#' @return Returns a data frame with all sampled functions.
#' @export
#' @importFrom magrittr %>%
#' @examples
#' simple_grammar <- create_grammar(a = "<b><op><c>, <a><op><b>, <a><op><c>, 1",
#'                                  b = "var, numeric",
#'                                  c = "var, numeric",
#'                                  op = "+, -")
#' \dontrun{
#' grammar_functions <- grammar_sampler(n = 10,
#'                                      grammar = simple_grammar,
#'                                      max_depth = 5,
#'                                      no_cores = 1,
#'                                      save = FALSE)
#' variables <- c("x", "y")
#' numbers <- seq(1, 10, 1)
#'
#' sampled_functions <- variable_input(functions = grammar_functions,
#'                                     variables = variables, numbers = numbers,
#'                                     n_iter = 100, no_cores = 1,
#'                                     var_string = "var", num_string = "numeric",
#'                                     seed = NULL, save = FALSE)
#'
#' }
variable_input <- function(functions, variables, numbers,
                           n_iter = 100, no_cores = NULL,
                           var_string = "var", num_string = "numeric",
                           seed = NULL, save = TRUE){

  if(is.data.frame(functions)){
    if(ncol(functions) == 1){
      functions <- functions[, 1]
    } else stop("functions must be either a character vector or a 1 column data frame with function strings.")
  } else {
    if(!is.character(functions)){
      stop("functions must be either a character vector or a 1 column data frame with function strings.")
    }
  }
  # NULL seed
  if(is.null(seed)) seed <- sample(1000000000, 1)

  # Calculate the number of cores if not specified
  if (is.null(no_cores)) {
    no_cores <- min(1, parallel::detectCores() - 2)
  }
  no_cores <- min(parallel::detectCores(), no_cores)

  # parallel sampling
  cat("Sampling varialbes and/or numerics", n_iter, "times with", no_cores, "cores:\n")
  if(Sys.info()[["sysname"]] == "Windows"){
    # use parlapply version for windows
    cl <- parallel::makeCluster(no_cores)
    parallel::clusterSetRNGStream(cl, iseed = seed)
    parallel::clusterExport(cl, list(".var_sampler", ".simple_grammar_sampler",
                                     "variables", "numbers", "create_grammar",
                                     "var_string", "num_string",
                                     "%>%",
                                     ".grammar_sample", "n_iter"),
                            envir = environment())
    output <- unlist(pbapply::pblapply(functions,
                                       FUN = .var_sampler,
                                       variables = variables,
                                       numbers = numbers,
                                       var_string = var_string, num_string = num_string,
                                       n_iter = n_iter,
                                       cl = cl))
    parallel::stopCluster(cl)
  } else {
    RNGkind("L'Ecuyer-CMRG")
    set.seed(seed)
    parallel::mc.reset.stream()
    output <- unlist(pbmcapply::pbmclapply(X = functions,
                                           FUN = .var_sampler,
                                           variables = variables,
                                           numbers = numbers,
                                           var_string = var_string, num_string = num_string,
                                           n_iter = n_iter,
                                           mc.cores = as.integer(no_cores)))
  }
  output <- unique(output)
  output <-  data.frame("functions" = output,
                        stringsAsFactors = FALSE)
  if(save){
    file_name <- paste0("variable_input_grammar", "-",
                        format(Sys.time(), "%d-%m-%Y-%H:%M"), ".feather")
    cat("Results are saved in",file_name)
    feather::write_feather(x = output,
                           path = file_name)
  }
  return(output)
}
