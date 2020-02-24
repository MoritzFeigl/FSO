#' grammar_sampler
#'
#' @param n Number of functions to be sampled from grammar
#' @param grammar A Grammar Object, created by the create_grammar() function.
#' @param max_depth Maximum recursive depth used to sample grammar.
#' @param no_cores The number of cores for parallel computation. If NULL than all but 2 cores are used.
#' @param save TRUE/FALSE, saves a feather of the output in the current working directory.
#' @param unique Should only the uniquely sampled functions be kept.
#'
#' @return Returns a character vector of all sampled functions.
#' @export
#'
#' @examples
#' simple_grammar <- create_grammar(a = "<b><op><c>, <a><op><b>, <a><op><c>, 1",
#'                                  b = "2, 4",
#'                                  c = "1, 3, 5",
#'                                  op = "+, -")
#' grammar_functions <- grammar_sampler(n = 300,
#'                                      grammar = simple_grammar,
#'                                      max_depth = 5)
grammar_sampler <- function(n,
                            grammar,
                            max_depth,
                            no_cores = NULL,
                            save = TRUE,
                            unique = TRUE,
                            seed = NULL){

  # check if grammar is recursive
  if(.is_not_recursive(grammar)) max_depth <- Inf

  if(is.null(seed)) seed <- sample(10000, 1)
  # Calculate the number of cores if not specified
  if (is.null(no_cores)) {
    no_cores <- min(1, parallel::detectCores() - 2)
  }
  no_cores <- min(parallel::detectCores(), no_cores)

  # Parallel run with progress bar
  cat("Sampling Grammar", n, "times with", no_cores, "cores:\n")
  if(Sys.info()[["sysname"]] == "Windows"){
    # use parlapply version for windows
    cl <- parallel::makeCluster(no_cores)
    parallel::clusterSetRNGStream(cl, iseed = seed)
    parallel::clusterExport(cl, list(".grammar_sample", "grammar", "%>%", "max_depth"),
                            envir = environment())
    output <- pbapply::pbreplicate(n = n,
                                   expr = .grammar_sample(grammar = grammar,
                                                          max_depth = max_depth),
                                   cl = cl)
    parallel::stopCluster(cl)
  } else {
    RNGkind("L'Ecuyer-CMRG")
    set.seed(seed)
    parallel::mc.reset.stream()
    output <- pbmcapply::pbmclapply(X = 1:n,
                                    FUN = function(x) .grammar_sample(grammar = grammar,
                                                                      max_depth = max_depth),
                                    mc.cores = as.integer(no_cores)) %>%
      unlist()
  }
  # take unique functions
  if(unique)  output <- unique(output)
  # make data frame
  output <-  data.frame("funtions" = output,
                        stringsAsFactors = FALSE)

  # save as feather
  if(save){
    feather::write_feather(x = output,
                           path = paste0("sampled_grammar", "-",
                                         format(Sys.time(), "%d-%m-%Y-%H:%M"), ".feather"))
  }
  return(output)
}


