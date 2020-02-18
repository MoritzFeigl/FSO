#' grammar_sampler
#'
#' @param Grammar    A Grammar Object, created by the grammar() function.
#' @param max.depth Maximum recursive depth used to create search Table.
#' @param n Number of functions to be created from Grammar.
#' @param save_feather TRUE/FALSE, saves a feather of the output list
#' @param parallel Should code be run parallel or not? TRUE or FALSE
#' @param no_cores The number of cores for parallel computation. If NULL than no_cores = detectCores() - 2, hence all but 2 cores.
#'
#' @return Returns a df with all sampled grammar vectors and their corresponding functions. Each row is 1 grammar vector and 1 resulting function (last element).
#' @export
#'
#' @examples
#' g_v1 <- grammar(tf = "numeric * <eq> + numeric, <eq> + numeric, <eq> ",
#'                 eq = "<fs>, <eq><op><fs>, <eq><op>numeric, <fs><op>numeric",
#'                 fs = "<sp>,  <f>(<sp>), <sp><op><sp>, numeric",
#'                 f = "exp, log",
#'                 op = "+, -, *, /",
#'                 sp = "slope, evi, sand, clay, elevation, hand, noise")
#' g_v1_table <- search.table(g_v1, max.depth = 3)
#' my_function_list <- par_grammar_sampler(n = 30000000,
#'                                         cfgram = g_v1,
#'                                         gram_table = g_v1_table
#'                                         max.depth = 5)
grammar_sampler <- function(Grammar,
                                max.depth,
                                n,
                                save_feather = TRUE,
                                parallel = TRUE,
                                no_cores = NULL){
  # prepare grammar and table
  gram_table <- search.table(cfgram, max.depth = max.depth)
  gram <- Grammar
  if(parallel){
    library(parallel)
    # Calculate the number of cores
    if(is.null(no_cores)){
      no_cores <- detectCores() - 2
    }
    ns <- rep(as.integer(n/no_cores), no_cores)
    # old parLapply version -> problem with seeds for different cores
    # Initiate cluster
    #cl <- makeForkCluster(no_cores, seed = sample(1000, size = no_cores))
    # functions_list <- parLapply(cl, ns, functions.creator,
    #                             gram = gram,
    #                             gram_table = gram_table,
    #                             depth = max.depth)

    # Parallel run with mclapply
    start <- Sys.time()
    cat("Run startet at: ", as.character(start))
    RNGkind("L'Ecuyer-CMRG")
    functions_list <- mclapply(ns, .functions_creator, mc.cores = no_cores,
                               gram = gram,
                               gram_table = gram_table,
                               depth = max.depth,
                               mc.set.seed = TRUE)
    end <- Sys.time()
    cat("\nParallelized run with ", sum(ns), " iterations needed ", (end-start)/60/60, "hours.")
    #stopCluster(cl)
    time_name <- substr(as.character(end), 1, 10)
    gram_vectors <- functions_list[[1]]$gram_vector
    functions_vector <- functions_list[[1]]$functions_vector
    for(i in 2:no_cores){
      gram_vectors <- cbind(gram_vectors, functions_list[[i]]$gram_vector)
      functions_vector <- c(functions_vector, functions_list[[i]]$functions_vector)
    }
  } else {
    start <- Sys.time()
    cat("Run startet at: ", as.character(start))
    functions <- .functions_creator(n, gram = gram, gram_table = gram_table, depth = max.depth)
    end <- Sys.time()
    cat("\nNot parallelized run with", n, "iterations needed", (as.numeric(end)-as.numeric(start))/60/60, "hours.")
    time_name <- substr(as.character(end), 1, 10)
    gram_vectors <- functions$gram_vectors
    functions_vector <- functions$functions_vector

  }


  functions_list <- list("Grammatic_vectors" = gram_vectors,
                         "transfer_functions" = functions_vector)
  # save as feather
  if(save_feather){
    feather::write_feather(functions_list, paste0("function_space", "-", time_name, ".csv"), row.names = FALSE)
  }

  functions_df <- data.frame(t(functions_list$Grammatic_vectors), functions_list$transfer_functions,
                             stringsAsFactors = FALSE)
  names(functions_df) <- c(gram_table$non.terminal, "Transfer_Function")
  return(functions_df)
}


