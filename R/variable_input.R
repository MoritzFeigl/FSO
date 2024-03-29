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
variable_input <- function(functions,
                           variable_list,
                           single_var_to_remove = NULL,
                           necessary_var_to_be_included = NULL,
                           n_iter = 100, no_cores = NULL,
                           seed = NULL,
                           file_name = NULL){

  # Check Inputs -------------------------------------------------------------------------
  # Load data if only the name of the file is given
  if(class(functions) == "character" & length(functions) == 1){
    if(!grepl(".fst", functions)) functions <- paste0(functions, ".fst")
    tryCatch(expr = {functions <- fst::read_fst(functions)},
             error = function(e) stop("Could not find ", functions)
    )
  }
  # check if functions are a character matrix
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
  # define file_name if not specified
  if(is.null(file_name)){
    file_name <- paste0("variable_input_grammar", "-",
                        format(Sys.time(), "%d-%m-%Y-%H%M"))
  }
  file_name_template <- tail(unlist(strsplit(file_name, "/")), 1)

  # create folder
  cat("Results will be saved in", file_name, "\n")
  if(!dir.exists(file_name))  dir.create(file_name)

  # in case ther is a single_var_to_remove chosen -> remove those
  functions <- functions[functions != single_var_to_remove]

  # Define batches for computation -------------------------------------------------------

  # since we only want unique functions it is necessary to have all iterations for a
  # single function in one output. Therefore, the computation splits nrow(functions)

  # dim each batch: functions_subsample * n_iter = nrow(functions)
  nrow_functions <- length(functions)
  batch_size <- ceiling(nrow_functions/n_iter)
  if((nrow_functions %% batch_size) != 0){
    last_batch_size <- nrow_functions %% batch_size
  } else last_batch_size <- batch_size

  number_of_batches <- ceiling(nrow_functions/batch_size)

  # setup seed for parallel processes
  if(!is.null(seed)) {
    set.seed(seed)
    seeds <- sample(1000000000, number_of_batches)
  }

  # parallel runs --------------------------------------------------------------------------
  # parallel sampling
  cat("Sampling variables and/or numerics", n_iter,
      "times for", nrow_functions, "functions with", no_cores, "cores:\n")

  # run computation over batches
  for(batch in 1:number_of_batches){
    # batch info
    if(number_of_batches > 1){
      cat(paste0("Computing batch ", batch, "/", number_of_batches, "\n"))
    }
    if(batch == number_of_batches) batch_size <- last_batch_size
    # functions part for this batch
    functions_subsample <- (1:batch_size) + (batch - 1) * batch_size
    # parallel computation
    cl <- parallel::makeCluster(no_cores)
    if(is.null(seed)){
      parallel::clusterSetRNGStream(cl, iseed = NULL)
    } else {
      parallel::clusterSetRNGStream(cl, iseed = seeds[batch])
    }
    parallel::clusterExport(cl, list(".var_sampler",
                                     "variable_list", ".create_grammar_from_list",
                                     "necessary_var_to_be_included",
                                     "%>%", ".recur",
                                     ".grammar_sample", ".rule"
    ),
    envir = environment())
    output <- pbapply::pblapply(functions[functions_subsample],
                                FUN = .var_sampler,
                                variable_list = variable_list,
                                necessary_var_to_be_included = necessary_var_to_be_included,
                                n_iter = n_iter,
                                cl = cl)
    parallel::stopCluster(cl)
    output <- unlist(output)
    file_name_batch <- paste0(file_name_template, "_batch", batch, ".fst")
    output <- output[!is.na(output)]
    output <- unique(output)
    output <-  data.frame("functions" = output,
                          stringsAsFactors = FALSE)
    fst::write_fst(x = output, path = paste0(file_name, "/", file_name_batch), compress = 100)
    rm(output)

  }
}







































