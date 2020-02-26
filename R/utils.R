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

.simplify_single_function <- function(fun, function_variables){
  ### simplifies a function string
  ### inputs: fun (character), function_variables (character)
  ### output: simplified function string (character)

  if (length(fun) > 1) stop("fun should be one character string.")
  # remove spaces
  fun <- gsub(" ", "", fun)
  nums <- length(grep("numeric", fun)) > 0 # are there "numeric" values

  if(nums){
    # If there are no function_variables -> return NA
    tf_splitted <- unlist(strsplit(fun, "(?=[+-/*)()])", perl = TRUE))

    # here starts the real function
    # which numerics are only sourrounded by +/- -> reduce it to just one + numeric
    tf_len <- nchar(fun)
    while(TRUE){
      now_len <- nchar(fun)
      if( length(grep("+numeric+", fun, fixed = TRUE)) > 0) fun <- gsub("+numeric+", "+", fun, fixed = TRUE)
      if( length(grep("-numeric+", fun, fixed = TRUE)) > 0) fun <- gsub("-numeric+", "+", fun, fixed = TRUE)
      if( length(grep("+numeric-", fun, fixed = TRUE)) > 0) fun <- gsub("+numeric-", "-", fun, fixed = TRUE)
      if( length(grep("-numeric-", fun, fixed = TRUE)) > 0) fun <- gsub("-numeric+", "+", fun, fixed = TRUE)

      if(substr(fun, nchar(fun) - 7, nchar(fun)) == "+numeric") {
        fun <- fun <- substr(fun, 1, nchar(fun) - 8)
      }
      if(substr(fun, nchar(fun) - 7, nchar(fun)) == "-numeric") {
        fun <- fun <- substr(fun, 1, nchar(fun) - 8)
      }
      if(substr(fun, 1, 8) == "numeric+") {
        fun <- substring(fun, 9)
      }
      if(substr(fun, 1, 8) == "numeric-") {
        fun <- substring(fun, 9)
      }
      if(now_len == nchar(fun)) break
    }
    # in case some numerics were removed -> add one +numeric
    if(tf_len != nchar(fun)) fun <- paste0(fun, "+numeric")

    for(i in 1:4){
      # replace obvious numerics
      fun <- gsub("numeric/numeric", "numeric", fun, fixed = TRUE)
      fun <- gsub("numeric*numeric", "numeric", fun, fixed = TRUE)

      # if a single numeric is between parentheses -> remove parantheses
      fun <- gsub("(numeric)", "numeric", fun, fixed = TRUE)
    }
    # split fun at +-
    fun <- unlist(strsplit(fun, "(?=[+-])", perl = TRUE))
    fun <- fun[fun != "0"]

    # are two numerics seperated by a /
    for(i in 1:length(fun)){
      sfun <- unlist(strsplit(fun[i], "(?=[/])", perl = TRUE))
      if(length(sfun) > 1){
        for(k in 1:length(sfun)) sfun[k] <- gsub("numeric", paste0("numeric", k), sfun[k])
        fun[i] <- paste0(sfun, collapse = "")
      }
    }

    for(i in 1:length(fun)) fun[i] <- gsub("numeric", paste0("numeric", i), fun[i])
    fun <- paste0(fun, collapse = "")
    if(substr(fun, 1, 1) %in% c("+", "-")) fun <- substring(fun, 2)
    if(substring(fun, nchar(fun)) %in% c("+", "-")) fun <- substr(fun, 1, nchar(fun)-1)
  }
  fun <- Deriv::Simplify(fun)
  fun <- Deriv::Simplify(fun)
  fun <- gsub(" ", "", fun)
  if(nums){
    fun <- gsub("numeric+[0-9]", "numeric", fun)
    fun <- gsub("numeric+[0-9]", "numeric", fun)
    fun <- gsub("numeric+[0-9]", "numeric", fun)
    # change numeric with power values to numerics
    fun <- gsub("numeric+\\^+[0-9]", "numeric", fun)
    fun <- gsub("numeric/numeric", "numeric", fun, fixed = TRUE)
    fun <- gsub("numeric*numeric", "numeric", fun, fixed = TRUE)
  }

  # clean numerics one more time
  if(nums){
    # Check if there are any obviously non important nums
    while(TRUE){
      now_len <- nchar(fun)
      if( length(grep("(numeric+numeric)", fun, fixed = TRUE)) > 0) fun <- gsub("numeric+numeric", "numeric", fun, fixed = TRUE)
      if( length(grep("(numeric-numeric)", fun, fixed = TRUE)) > 0) fun <- gsub("numeric+numeric", "numeric", fun, fixed = TRUE)
      if(now_len == nchar(fun)) break
    }

    tf_len <- nchar(fun)
    while(TRUE){
      now_len <- nchar(fun)
      if( length(grep("+numeric+", fun, fixed = TRUE)) > 0) fun <- gsub("+numeric+", "+", fun, fixed = TRUE)
      if( length(grep("-numeric+", fun, fixed = TRUE)) > 0) fun <- gsub("-numeric+", "+", fun, fixed = TRUE)
      if( length(grep("+numeric-", fun, fixed = TRUE)) > 0) fun <- gsub("+numeric-", "-", fun, fixed = TRUE)
      if( length(grep("-numeric-", fun, fixed = TRUE)) > 0) fun <- gsub("-numeric+", "+", fun, fixed = TRUE)

      if(substr(fun, nchar(fun) - 7, nchar(fun)) == "+numeric") {
        ffun <- substr(fun, 1, nchar(fun) - 8)
      }
      if(substr(fun, nchar(fun) - 7, nchar(fun)) == "-numeric") {
        fun <- substr(fun, 1, nchar(fun) - 8)
      }
      if(substr(fun, 1, 8) == "numeric+") {
        fun <- substring(fun, 9)
      }
      if(substr(fun, 1, 8) == "numeric-") {
        fun <- substring  (fun, 9)
      }
      if(now_len == nchar(fun)) break
    }
    # in case some numerics were removed -> add one +numeric
    if(tf_len != nchar(fun)) fun <- paste0(fun, "+numeric")
  }

  # did simplifying remove spatial predictors? test if there are still some
  # If there are no spatial_predictors -> return NA
  tf_splitted <- unlist(strsplit(fun, "(?=[+-/*()^])", perl = TRUE))
  if(sum(function_variables %in% tf_splitted) == 0) return(NA)
  fun <- gsub("*", " * ", fun, fixed = TRUE)
  fun <- gsub("+", " + ", fun, fixed = TRUE)
  fun <- gsub("-", " - ", fun, fixed = TRUE)
  return(fun)
}

.trampoline <- function(f, ...) {
  function(...) {
    ret <- f(...)
    while (inherits(ret, "recursion")) {
      ret <- eval(as.call(c(f, unclass(ret))))
    }
    ret
  }
}

.recur <- function(...) {
  structure(list(...), class = "recursion")
}

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

.is_not_recursive <- function(grammar){
  # Recursive or not?
  nonterm <- names(grammar)
  rec <- vector(mode ="list", length = length(grammar))
  for(i in 1:length(grammar)){
    rec[[i]] <- grep(nonterm[i], grammar[[i]])
  }
  return(length(unlist(rec)) == 0)
}

.double_numeric_remover <- .trampoline(function(fun, n_iter = 4) {
  depth_counter <<- ifelse(exists("depth_counter"), depth_counter + 1, 1)
  fun <- gsub("numeric+numeric", "numeric", fun, fixed = TRUE)
  fun <- gsub("numeric-numeric", "numeric", fun, fixed = TRUE)
  fun <- gsub("numeric*numeric", "numeric", fun, fixed = TRUE)
  fun <- gsub("numeric/numeric", "numeric", fun, fixed = TRUE)
  if(depth_counter == n_iter) {
    rm("depth_counter", envir = .GlobalEnv)
    return(fun)
  }
  return(.recur(fun = fun, n_iter = n_iter))
})

.simple_grammar_sampler <- function(n, grammar, max_depth){
output <- replicate(n = n,
          expr = .grammar_sample(grammar = grammar,
                                 max_depth = max_depth))
return(output)
}

.var_sampler <- function(fun, variables, numbers, n_iter,
                         var_string, num_string){
  fun <- gsub(num_string, "<numeric>", fun)
  fun <- gsub(var_string, "<var>", fun)
  var_num_grammar <- create_grammar(fun = fun,
                                    numeric = numbers,
                                    var = variables)
  sampled_functions <- .simple_grammar_sampler(n = n_iter,
                                               grammar = var_num_grammar,
                                               max_depth = Inf)
  return(sampled_functions)
}
