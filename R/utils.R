.rule <- function(rule_string) {
  #### Function to turn strings into grammar rules
  #### inputs: rule_string (character)
  #### output: rule (list)

  # remove spaces
  rule_string <- gsub(" ", "", rule_string)
  # split by comma
  rule <- unlist(strsplit(rule_string, split = ","))
  # find parantheses and fix possible wrong splits
  ind_par_open <- grep(pattern = "(", rule, fixed = TRUE)
  if(length(ind_par_open) != 0){
    ind_par_close <- grep(pattern = ")", rule, fixed = TRUE)
    if(length(ind_par_open) != length(ind_par_close)){
      stop("Rule formulation error. Single parenthesis found.")
    }
    del <- integer()
    for(i in seq_along(ind_par_open)){
      if(ind_par_open[i] != ind_par_close[i]){
        rule[ind_par_open[i]] <- paste0(rule[ind_par_open[1]], rule[ind_par_close[i]])
        del <- c(del, ind_par_close[i])
      }
    }
    # delete wrong splits
    if(length(del != 0)) rule <- rule[-del]
  }

  return(rule)
}
.ruler <- compiler::cmpfun(.rule)

.trampoline <- function(f, ...) {
  function(...) {
    ret <- f(...)
    while (inherits(ret, "recursion")) {
      ret <- eval(as.call(c(f, unclass(ret))))
    }
    ret
  }
}
.trampoline <- compiler::cmpfun(.trampoline)

.recur <- function(...) {
  structure(list(...), class = "recursion")
}
.recur <- compiler::cmpfun(.recur)

.grammar_sample <- .trampoline(function(grammar, max_depth, fun = NULL) {
  if(is.null(fun)){
    fun <- names(grammar)[1]
  }
  # add count
  depth_counter <<- ifelse(exists("depth_counter"), depth_counter + 1, 1)

  # get nonterminal
  nonterminals <- unlist(regmatches(fun, gregexpr("<(.*?)>", fun)))

  # remove unsolved nonterminals from fun
  regmatches(fun, gregexpr("<(.*?)>", fun)) <- "%$&"

  # max_number -f counts is reached
  if (depth_counter == max_depth) {
    # for every remaining nonterminal find the closest set of terminals or put NA
    for(i in seq_along(nonterminals)){
      teminals_id <- grep("<(.*?)>", grammar[[nonterminals[i]]], invert = TRUE)
      next_terminals <- grammar[[nonterminals[i]]][teminals_id]
      if(length(next_terminals) == 0) next_terminals <- "NA"
      fun <- sub("%$&", replacement = sample(next_terminals, 1), fun, fixed = TRUE)
    }
    rm("depth_counter", envir = .GlobalEnv)
    return(fun)
  }

  # in case no nonterminals exist return function
  if (length(nonterminals) == 0) {
    rm("depth_counter", envir = .GlobalEnv)
    return(fun)
  }

  # get values for nonterminals and plug in to fun again
  for(i in seq_along(nonterminals)) {
    sample_id <- sample.int(length(grammar[[nonterminals[i]]]), 1)
    nonterminals[i] <- grammar[[nonterminals[i]]][sample_id]
    fun <- sub("%$&", replacement = nonterminals[i], fun, fixed = TRUE)
  }
  return(.recur(fun = fun, grammar = grammar, max_depth = max_depth))
})
.grammar_sample <- compiler::cmpfun(.grammar_sample)

.is_not_recursive <- function(grammar){
  # Recursive or not?
  nonterm <- names(grammar)
  rec <- vector(mode ="list", length = length(grammar))
  for(i in 1:length(grammar)){
    rec[[i]] <- grep(nonterm[i], grammar[[i]])
  }
  return(length(unlist(rec)) == 0)
}

.double_numeric_remover <- .trampoline(function(fun, num_string) {
  # put in chosen numeric string
  fun <- gsub(num_string, "numeric", fun, fixed = TRUE)
  # simplify
  depth_counter <<- ifelse(exists("depth_counter"), depth_counter + 1, 1)
  fun <- gsub("numeric+numeric", "numeric", fun, fixed = TRUE)
  fun <- gsub("numeric-numeric", "numeric", fun, fixed = TRUE)
  fun <- gsub("numeric*numeric", "numeric", fun, fixed = TRUE)
  fun <- gsub("numeric/numeric", "numeric", fun, fixed = TRUE)
  if(depth_counter == 6) {
    rm("depth_counter", envir = .GlobalEnv)
    return(fun)
  }
  return(.recur(fun = fun, num_string = num_string))
})
.double_numeric_remover <- compiler::cmpfun(.double_numeric_remover)

.create_grammar_from_list <- function(def) {
  # rename non-terminals
  names(def) <- paste0("<", names(def), ">")
  # apply rule function
  def_gram <- lapply(def, .rule)
  def_gram <- lapply(def_gram, function(x) gsub("\n", "", x))
  return(def_gram)
}

.var_sampler <- function(fun, variable_list, necessary_var_to_be_included,
                         numbers, n_iter){
  if(!grepl(necessary_var_to_be_included, fun)) return(NA)
  var_names <- names(variable_list)
  for(i in seq_along(var_names)){
    fun <- gsub(var_names[i],
                paste0("<", var_names[i], ">"),
                fun)
  }

  var_num_grammar <-  .create_grammar_from_list(c("fun" = fun, variable_list))
  sampled_functions <- replicate(n = n_iter,
                                 expr = .grammar_sample(grammar = var_num_grammar,
                                                        max_depth = Inf))
  return(sampled_functions)
}
.var_sampler <- compiler::cmpfun(.var_sampler)

.range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

.evaluate_function_from_string <- function(string, variable_df){
  # evaluates a function given as a string for all relevant variables
  # Input:
  #    string: transfer function as string
  #    variables: data frame with variable values
  # Output:
  #    vector with all evaluated function outputs
  tf <- unlist(strsplit(string, c("[/^()*+-]")))
  tf <- gsub(" ", "", tf, fixed = TRUE)
  relevant_predictors <- variable_df[which(names(variable_df) %in% tf)]
  if(ncol(relevant_predictors) == 0){
    args <- ""
    eval(parse(text = paste('f <- function(', args, ') { return(' , string , ')}', sep='')))
    f_evaluated <- rep(f(), nrow(variable_df))
  } else {
    args <- paste(names(relevant_predictors), collapse = ', ')

    eval(parse(text = paste('f <- function(', args, ') { return(' , string , ')}', sep='')))
    suppressWarnings(
      f_evaluated <- eval(parse(text = paste('mapply(f, ',
                                             paste0('relevant_predictors$',
                                                    names(relevant_predictors), collapse = ', '),
                                             ')')))
    )
  }
  f_evaluated <- f_evaluated[is.finite(f_evaluated)]
  return(f_evaluated)
}
.evaluate_function_from_string <- compiler::cmpfun(.evaluate_function_from_string)

.distribution_values_from_fun <- function(string, variable_df){
  # tf_evaluated <- .evaluate_function_from_string(string = string,
  #                                                variable_df = variable_df)

  # fix scientific notation bug
  strs <- unlist(strex::str_split_by_numbers(string, sci=TRUE, decimals = TRUE))
  remove <- which(strs == "00")
  if(length(remove) > 0) strs <- strs[-c(remove, remove-1)]
  string <- paste0(strs, collapse ="")

  # check if variable is inside
  string <- gsub(" ", "", string)
  tf <- unlist(strsplit(string, c("[/^()*+-]")))
  tf <- gsub(" ", "", tf, fixed = TRUE)
  relevant_predictors <- variable_df[which(names(variable_df) %in% tf)]

  if(ncol(relevant_predictors) == 0){
    args <- ""
    try({eval(parse(text =
                    paste('f <- function(', args, ') { return(' , string , ')}', sep='')))
    })
    f_evaluated <- try(rep(f(), nrow(variable_df)), silent = TRUE)
  } else {
    args <- paste(names(relevant_predictors), collapse = ', ')

    try({eval(parse(text =
                    paste('f <- function(', args, ') { return(' , string , ')}', sep='')))
    })
    suppressWarnings(
      f_evaluated <- try(eval(parse(text = paste('mapply(f, ',
                                             paste0('relevant_predictors$',
                                                    names(relevant_predictors), collapse = ', '),
                                             ')'))), silent = TRUE
    ))
   # f_evaluated<- format(f_evaluated, scientific = FALSE)
  }
  if(class(f_evaluated) == "try-error"){
    distribution_values <- data.frame(string,
                                      t(rep(NA, 9)), stringsAsFactors = FALSE)
    names(distribution_values) <- c("function", "10%", "20%", "30%", "40%", "median",
                                    "60%", "70%", "80%", "90%")
    return(distribution_values)
  }
  #f_evaluated <- f_evaluated[is.finite(f_evaluated)]
  # calculate quantiles
  if(length(f_evaluated) == 0) {
    distribution_values <- data.frame(string,
                                      t(rep(NA, 9)), stringsAsFactors = FALSE)
    names(distribution_values) <- c("function", "10%", "20%", "30%", "40%", "median",
                                    "60%", "70%", "80%", "90%")
    return(distribution_values)
  }

  quants <- round(quantile(f_evaluated,
                           probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
                           na.rm = TRUE), 4)

  distribution_values <- data.frame(string,
                                    t(quants), stringsAsFactors = FALSE)
  names(distribution_values) <- c("function", "10%", "20%", "30%", "40%", "median",
                                  "60%", "70%", "80%", "90%")

  return(distribution_values)
}
.distribution_values_from_fun <- compiler::cmpfun(.distribution_values_from_fun)

.simplify <- compiler::cmpfun(Deriv::Simplify)
# .yac_str <- compiler::cmpfun(Ryacas::yac_str)

.str_format <- compiler::cmpfun(function(x){
  #if(!is.na(as.numeric(x))) x <- round(as.numeric(x),2) %>% format(nsmall=2)
  if(!is.na(as.numeric(x))){
    x <- round(as.numeric(x)/0.05, 0)*0.05
    if(x > 3) x <- 3
    x <- x %>% format(nsmall=2)
  }
  return(x)})
# .simplifier_dec <- function(x){
#   out <- paste0("N(", x, ", ", 2L, ")") %>%
#     .yac_str()
#
#   try({
#     out <- .simplify(x) %>%
#     paste0("N(", ., ", ", 2, ")") %>%
#     .yac_str()
#
#     out <- suppressWarnings(
#       gsub("log10", "log*", out, fixed = TRUE) %>%
#         strsplit(split = "(?<=[^0-9.])(?=\\d)|(?<=\\d)(?=[^0-9.])", perl=T) %>%
#         unlist %>%
#         lapply(.str_format) %>%
#         paste(collapse="") %>%
#         gsub("log*", "log10", ., fixed = TRUE)
#     )
#   })
#   return(out)
# }
.simplifier_dec <- function(x){
  try({
    x <- .simplify(x)
    x <-  suppressWarnings(
      gsub("log10", "log*", x, fixed = TRUE) %>%
        strsplit(split = "(?<=[^0-9.])(?=\\d)|(?<=\\d)(?=[^0-9.])", perl=T) %>%
        unlist %>%
        lapply(.str_format) %>%
        paste(collapse="") %>%
        gsub("log*", "log10", ., fixed = TRUE)
)
  })
  return(x)
}
.simplifier_dec <- compiler::cmpfun(.simplifier_dec)

.prepare_tf_for_mhm <- function(tf, scaling_bounds){
  for(i in 1:length(scaling_bounds)){
    scaling_bounds[[i]] <- scaling_bounds[[i]] %>% format(nsmall = 1) %>% gsub(" ", "", .)
  }

  # prepare
  tf <- gsub(" ", "", tf)
  tf <- gsub("^", "**", tf, fixed = TRUE)
  # split tf
  tf_splitted <- tf %>%
    strsplit(., c("[/^()*+-]")) %>%
    unlist()
  tf_splitted <- tf_splitted[tf_splitted != ""]
  var <- character()
  variables <- names(scaling_bounds)
  for(i in seq_along(tf_splitted)){
    if(tf_splitted[i] %in% variables)  var <- c(var, tf_splitted[i])
  }
var <- unique(var)
  # change variables to scaled variables in tf
  for(variable in var){
    bounds <- scaling_bounds[[variable]]
    tf <- gsub(pattern = variable,
               replacement = paste0("((", variable, "-", bounds[1], ")/(",
                                    bounds[2], "-", bounds[1], "))"),
               tf)
  }
  # get numerical values
  tf_splitted <- tf %>%
    strsplit(., c("[/^()*+-]")) %>%
    unlist()
  tf_splitted <- tf_splitted[tf_splitted != ""]
  numerics <- character()
  suppressWarnings(
    for(i in seq_along(tf_splitted)){
      if(!is.na(as.numeric(tf_splitted[i]))) numerics <- c(numerics, tf_splitted[i])
    }
  )
  # make integers to float
  ints <- grep(".", numerics, invert = TRUE, fixed = TRUE)
  numerics[ints] <- paste0(numerics[ints], ".0")

  # put space into function
  tf <- tf %>%
   gsub("+", " + ", ., fixed = TRUE) %>%
   gsub("-", " - ", ., fixed = TRUE) %>%
   gsub("*", " * ", ., fixed = TRUE) %>%
   gsub("*  *", "**", ., fixed = TRUE) %>%
   gsub("/", " / ", ., fixed = TRUE)

if(substr(tf, 1, 1) == " ") tf <- substring(tf, 2)
  return(list("scaled_function" = tf, "numerics" = numerics))
}

.function_splitter <- function(point_tf){
  function_splitted <- unlist(strsplit(point_tf, c("[/^()*+-]")))
  function_splitted <- gsub(" ", "", function_splitted)
  function_splitted <- function_splitted[function_splitted != ""]
  return(function_splitted)
}

.dds <- function(xBounds.df, numIter, OBJFUN, search_dim, start_point = NULL){
  # INPUTS:
  # xBounds.df must be a dataframe with 1st column as minimum, 2nd column as maximum
  # numIter is an integer
  # OBJFUN is a function which returns a scalar value, for which we are trying to minimize.
  #
  # OUTPUTS:
  # outputs.df is a two entry list, containing x_best and y_best, as they evolve over numIter iterations.

  # Format xBounds.df colnames
  colnames(xBounds.df) <- c("min", "max")
  if(is.null(start_point)){
  # Generate initial first guess that is not NA
  x_init <- rnorm(search_dim)
  } else {
    cat("Using pre-defined start point for optimization\n")
    x_init <- start_point
  }
  # Evaluate first cost function
  x_evaluated <- OBJFUN(x_init)
  while(is.na(x_evaluated$loss)){
    x_init <- rnorm(search_dim)
    x_evaluated <- OBJFUN(x_init)
  }

  x_ini <- x_evaluated$`Current point in function space`
  x_best <- matrix(x_init, nrow = 1)
  if(!is.na( x_evaluated$loss)){
    y_init <- x_evaluated$loss
  } else {
    y_init <- -999
  }
  y_best <- y_init
  r = 0.2
  # Select which entry to peturb at each iteration
  peturbIdx <- .probPeturb(xBounds.df, numIter)
  # Peturb each entry by N(0,1)*r(x_max - x_min) reflecting if @ boundaries
  sigma <- xBounds.df$max - xBounds.df$min
  for (i in 2:numIter){
    # Set up test x
    x_test <- x_best[i-1, ]
    # Get entries we will peturb
    idx <- peturbIdx[[i]]
    # Initialize vector of peturbations initially zeros with same length of x so we will add this vector to peturb x
    peturbVec <- rep(0, length(x_test))
    # Generate the required number of random normal variables
    N <- rnorm(length(x_test), mean=0, sd=1)
    # Set up vector of peturbations
    peturbVec[idx] <- r*N[idx]*sigma[idx]
    # Temporary resulting x value if we peturbed it
    testPeturb <- x_test + peturbVec
    # Find the values in testPeturb OBJFUN <- wrapper_ofthat have boundary violations.  Store the indices in boundaryViolationsIdx
    boundaryViolationIdx <- which(testPeturb<xBounds.df$min | testPeturb > xBounds.df$max)
    # Reset those violated indices to the opposite peturbation direction
    peturbVec[boundaryViolationIdx]<-(-1*r*N[boundaryViolationIdx]*sigma[boundaryViolationIdx])
    # Find values still at violations of min or max and set them to the minimum or maximum values
    testPeturb <- x_test + peturbVec
    minViolationIdx <- which(testPeturb<xBounds.df$min)
    maxViolationIdx <- which(testPeturb>xBounds.df$max)
    testPeturb[minViolationIdx] <- xBounds.df$min[minViolationIdx]
    testPeturb[maxViolationIdx] <-xBounds.df$max[maxViolationIdx]
    # Peturb the test vector
    x_test <- x_test + peturbVec
    # Evaluate objective function #§ a bit sloppy.. but never mind...
    x_evaluated <- OBJFUN(x_test)
    x_test <- x_evaluated$`Current point in function space`
    y_test <- x_evaluated$loss
    if(!is.na(y_test)) {
      y_best[i] <- max(c(y_test, y_best[i-1]))
      bestIdx <- which.max(c(y_test, y_best[i-1]))
    } else {
      y_best[i] <- y_best[i-1]
      bestIdx <- 2
    }
    x_choices <- cbind(x_test, x_best[i-1, ])
    x_best <- rbind(x_best, x_choices[,bestIdx])
  }
  output.list <- list(t(x_best), y_best)
  return(output.list)
}

.probPeturb <- function(x, numIter){
  # Returns numIter length list of entries to be peturbed
  # Input is xBounds & numIter.
  # Returns numIter entry list with the indices which will be peturbed
  xDims <- nrow(x)
  probabilityVector <- 1-log(1:numIter)/log(numIter)
  peturbIdx <- probabilityVector %>%
    lapply(function(x) as.logical(rbinom(xDims, 1, x))) %>%
    unlist() %>%
    matrix(byrow = TRUE, ncol = xDims) %>%
    apply(MARGIN = 1, FUN = which)
  return(peturbIdx)
}

