#' variable_input
#'
#' @param functions
#' @param variables
#' @param numbers
#' @param n_iter
#' @param no_cores
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
variable_input <- function(functions, variables, numbers,
                           n_iter = 100, no_cores = NULL, seed = NULL, save = TRUE){

  if(is.data.frame(functions)){
    if(ncol(functions) == 1){
      functions <- as.character(functions)
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
                                     ".grammar_sample", "n_iter"),
                            envir = environment())
    output <- unlist(pbapply::pblapply(functions,
                                       FUN = .var_sampler,
                                       variables = variables,
                                       numbers = numbers,
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
