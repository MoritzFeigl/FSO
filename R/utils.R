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
compiler::cmpfun(.trampoline)

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

.var_sampler <- function(fun, variables, numbers, n_iter,
                         var_string, num_string){
  fun <- gsub(num_string, "<numeric>", fun)
  fun <- gsub(var_string, "<var>", fun)
  var_num_grammar <-  create_grammar(fun = fun,
                                    numeric = numbers,
                                    var = variables)
  sampled_functions <- replicate(n = n_iter,
                      expr = .grammar_sample(grammar = var_num_grammar,
                                             max_depth = Inf))
  return(sampled_functions)
}
.var_sampler <- compiler::cmpfun(.var_sampler)
