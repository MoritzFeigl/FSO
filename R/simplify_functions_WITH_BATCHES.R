#' simplify_functions
#'
#' @param files File paths to fst files containing functions that should be simplified.
#' @param no_cores Number of cores for parallel computations.
#'
#' @return
#' @export
#'
#' @examples
simplify_functions <- function(files, batches = 10, no_cores){

  # Calculate the number of cores if not specified
  if(is.null(no_cores)) {
    no_cores <- min(1, parallel::detectCores() - 2)
  }
  no_cores <- min(parallel::detectCores(), no_cores)

  count <- 0
  for(current_file in files){
    if(!grepl(".fst", current_file)) current_file <- paste0(current_file, ".fst")
    tryCatch(expr = {functions <- fst::read_fst(current_file)},
             error = function(e) stop("Could not find ", current_file, " in the current working directory")
    )
    if(batches > 1){
      batch_cuts <- cut(1:nrow(functions), breaks = batches)
      functions_batches <- split.data.frame(functions, batch_cuts)
    } else {
      functions_batches <- list(functions)
    }
    # parallel runs
    for(batch in 1:batches){
      count <- count + 1
      cat(paste0(count, "/", length(functions_batches)*length(files)), ": Simplify functions with", no_cores, "cores", ":\n")

      output <- parallel::mclapply(X = functions_batches[[batch]][, 1],
                                   FUN = .simplifier_dec, mc.cores = no_cores)
      output <- unlist(output)
      output <- output[!is.na(output)]
      output <- unique(output)
      output <-  data.frame("functions" = output,
                            stringsAsFactors = FALSE)
      file_name <- paste0(gsub(".fst", "", current_file, fixed = TRUE),
                          "_simplified_", batch, ".fst")
      fst::write_fst(x = output, path = file_name, compress = 100)
      rm(output)
    }
    simplified_file <- fst::read_fst(paste0(gsub(".fst", "", current_file, fixed = TRUE),
                                            "_simplified_", 1, ".fst"))
    colnames(simplified_file) <- NULL
    simplified_file <- as.character(simplified_file[, 1])
    for(batch in 2:batches){
      batch <- fst::read_fst(paste0(gsub(".fst", "", current_file, fixed = TRUE),
                                    "_simplified_", batch, ".fst"))
      batch <- as.character(batch[, 1])
      simplified_file <- c(simplified_file, batch)
    }
    file.remove(paste0(gsub(".fst", "", current_file, fixed = TRUE),
                       "_simplified_", 1:batches, ".fst"))
    simplified_file <- data.frame("functions" = simplified_file, stringsAsFactors = FALSE)
    file_name_all <- paste0(gsub(".fst", "", current_file, fixed = TRUE),
                            "_simplified.fst")
    fst::write_fst(x = simplified_file, path = file_name_all, compress = 100)
  }
}
