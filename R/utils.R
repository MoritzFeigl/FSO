.rule <- function(rule_string) {
  #### Function to turn strings into gramar rules
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
    f_evaluated <- eval(parse(text = paste('mapply(f, ',
                                           paste0('relevant_predictors$',
                                                  names(relevant_predictors), collapse = ', '),
                                           ')')))
  }
  return(f_evaluated)
}

.distribution_values_from_fun <- function(string, variable_df, cut_off = NULL){
  tf_evaluated <- .evaluate_function_from_string(string = string,
                                                 variable_df = variable_df)
  # if(!is.null(cut_off)){
  #   tf_evaluated[tf_evaluated < cut_off[1]] <- cut_off[1]
  #   tf_evaluated[tf_evaluated > cut_off[2]] <- cut_off[2]
  # }
  distribution_values <- round(c(
    quantile(tf_evaluated, probs = c(0.1, 0.2, 0.3, 0.4), na.rm = TRUE),
    mean(tf_evaluated, na.rm = TRUE),
    quantile(tf_evaluated, probs = c(0.6, 0.7, 0.8, 0.9), na.rm = TRUE)
  ), 4)

  names(distribution_values) <- c("10%", "20%", "30%", "40%", "mean",
                                  "60%", "70%", "80%", "90%")
  return(distribution_values)
}
