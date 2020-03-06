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
                           var_string = "var", num_string = "numeric",
                           n_iter = 100, no_cores = NULL,
                           seed = NULL,
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
  # NULL seed
  if(is.null(seed)) seed <- sample(1000000000, 1)

  # Calculate the number of cores if not specified
  if (is.null(no_cores)) {
    no_cores <- min(1, parallel::detectCores() - 2)
  }
  no_cores <- min(parallel::detectCores(), no_cores)

  if(is.null(file_name)){
    file_name <- paste0("variable_input_grammar", "-",
                        format(Sys.time(), "%d-%m-%Y-%H%M"))
  }
  cat("Results will be saved in", file_name, "\n")

  if(!dir.exists(file_name))  dir.create(file_name)

  # parallel sampling
  cat("Sampling variables and/or numerics", n_iter,
      "times for", nrow(functions), "functions with", no_cores, "cores:\n")
  # If necessary define batches for computation
  if(n_iter > 5){
      batch_size <- 5
      number_of_batches <- ceiling(n_iter/batch_size)
      if(n_iter %% batch_size != 0){
    last_batch_size <- n_iter %% batch_size
      } else last_batch_size <- batch_size
  } else {
    number_of_batches <- 1
    batch_size <- n_iter
  }
  set.seed(seed)
  seeds <- sample(1000000000, number_of_batches)
  # run computation over batches
  for(batch in 1:number_of_batches){
    if(number_of_batches > 1){
      cat(paste0("Computing batch ", batch, "/", number_of_batches, "\n"))
    }
    if(batch == number_of_batches) batch_size <- last_batch_size

    cl <- parallel::makeCluster(no_cores)
    parallel::clusterSetRNGStream(cl, iseed = seeds[batch])
    parallel::clusterExport(cl, list(".var_sampler",
                                     "variables", "numbers", "create_grammar",
                                     "var_string", "num_string",
                                     "%>%",
                                     ".grammar_sample", "batch_size", ".rule",
                                     ".recur"),
                            envir = environment())
    output <- pbapply::pbsapply(functions,
                                FUN = .var_sampler,
                                variables = variables,
                                numbers = numbers,
                                var_string = var_string, num_string = num_string,
                                n_iter = batch_size,
                                cl = cl)
    parallel::stopCluster(cl)
    file_name_batch <- paste0(file_name, "_batch", batch, ".fst")
    output <- unique(output)
    output <-  data.frame("functions" = output,
                          stringsAsFactors = FALSE)
    write_fst(x = output, path = paste0(file_name, "/", file_name_batch), compress = 100)


  }
}
