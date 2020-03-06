#' simplify_functions
#'
#' @param functions
#' @param numeric_string
#'
#' @return
#' @export
#'
#' @examples
simplify_numerics <- function(functions,
                              num_string = "numeric",
                              no_cores = NULL,
                              save = TRUE,
                              file_name = NULL){

  if(class(functions) == "character" & length(functions) == 1){
    if(!grepl(".fst", functions)) functions <- paste0(functions, ".fst")
    functions <- read_fst(functions)
  }

  if(is.data.frame(functions)){
    if(ncol(functions) == 1){
      functions <- as.matrix(functions)[, 1]
    } else stop("functions must be either a character vector or a 1 column data frame with function strings.")
  } else {
    if(!is.character(functions)){
      stop("functions must be either a character vector or a 1 column data frame with function strings.")
    }
  }
  # Calculate the number of cores if not specified
  if (is.null(no_cores)) {
    no_cores <- min(1, parallel::detectCores() - 2)
  }
  no_cores <- min(parallel::detectCores(), no_cores)

  cat("Simplify numerics with", no_cores, "cores:\n")
  if(Sys.info()[["sysname"]] == "Windows"){
    # use parlapply version for windows
    cluster <- parallel::makeCluster(no_cores)
    parallel::clusterSetRNGStream(cluster, iseed = NULL)
    parallel::clusterExport(cluster, c(".double_numeric_remover", "num_string",
                                     "functions"),
                            envir = environment())
    output <- pbapply::pbsapply(X = functions,
                                FUN = .double_numeric_remover,
                                num_string = num_string,
                                cl = cluster)
    parallel::stopCluster(cluster)
  } else {
    output <- unlist(pbmcapply::pbmclapply(X = functions,
                                           FUN = .double_numeric_remover,
                                           num_str = num_string,
                                           mc.cores = as.integer(no_cores)))
  }

  output <- unique(output)
  output <-  data.frame("functions" = output,
                        stringsAsFactors = FALSE)
  if(save){
    if(is.null(file_name)){
      file_name <- paste0("simple_grammar", "-",
                          format(Sys.time(), "%d-%m-%Y-%H%M"), ".fst")
    } else {
      file_name <- paste0(file_name, ".fst")
    }

    cat("Results are saved in", file_name)
    write_fst(x = output, path = file_name, compress = 100)
  }
  return(output)
}


