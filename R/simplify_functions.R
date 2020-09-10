#' simplify_functions
#'
#' @param files File paths to fst files containing functions that should be simplified.
#' @param no_cores Number of cores for parallel computations.
#'
#' @return
#' @export
#'
#' @examples
simplify_functions <- function(files, no_cores){
  # Calculate the number of cores if not specified
  if(is.null(no_cores)) {
    no_cores <- min(1, parallel::detectCores() - 2)
  }
  no_cores <- min(parallel::detectCores(), no_cores)
  #initial counter
  count <- 0
  # run simplify in parallel for each file
  for(current_file in files){
    if(!grepl(".fst", current_file)) current_file <- paste0(current_file, ".fst")
    tryCatch(expr = {functions <- fst::read_fst(current_file)},
             error = function(e) stop("Could not find ", current_file)
    )
    count <- count + 1
    cat(paste0(count, "/", length(files)), ": Simplify functions with",
        no_cores, "cores", ":\n")
    invisible(capture.output(output <- parallel::mclapply(X = functions[, 1],
                                                          FUN = .simplifier_dec,
                                                          mc.cores = no_cores)))
    output <- unlist(output)
    output <- output[!is.na(output)]
    output <- unique(output)
    output <-  data.frame("functions" = output,
                          stringsAsFactors = FALSE)
    file_name <- paste0(gsub(".fst", "", current_file, fixed = TRUE),
                        "_simplifie.fst")
    fst::write_fst(x = output, path = file_name, compress = 100)
    rm(output)
  }
}
