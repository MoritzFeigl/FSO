#' generate_function
#'
#' Applies search table on a Grammar object
#' @param Grammar Grammar object produced by the function grammar
#' @param grammar_table search table produced by grammar_table and filled with integer values larger 0
#' @param max.depth Maximum recursive depth given as an integer.
#'
#' @return string of function call
#' @export
#'
#' @examples
#' test <- grammar(a = "<b> + 1, <c>(<b>)",
#'                 b = "1 + <c>(3), 2",
#'                 c = "log, exp, max")
#'
#' test_table <- grammar_table(test)
#' test_table$value <- c(2, 1, 1, 1)
#'
#' apply.search(test, test_table)
#'
#' recursive_grammar <- grammar(a = "<a> + <b>, <c>(<a>)",
#'                              b = "1 + <c>(3), <b>",
#'                              c = "log, exp")
#' recursive_table <- grammar_table(recursive_grammar, max.depth = 4)
#' recursive_table$value <- 1
#'
#' # This recursive table does not work without specifying the max.depth also in the apply.search function
#' # This is due to the fact that in comlicated recursive function, it is hard to infere the max depth from the search table
#' generate_function(recursive_grammar, recursive_table)
#'
#' # Now it works
#' apply.search(recursive_grammar, recursive_table, max.depth = 4)
#'
#'
generate_function <- function(Grammar,
                              grammar_table){
  # remove space from grammar
  Grammar <- lapply(Grammar, function(x) gsub(" ", "", x))
  # find out max.depth from given search table
  tryCatch({
    nr_nt <- gsub("[^0-9]","", grammar_table$non.terminal)
  }, error = function(cond) {
    stop("The search table does not have an non.terminal column. Use the create_grammar_table function to generate a template table for your Grammar.",
         call. = FALSE)
  })
  # set max.depth
  if(sum(nr_nt != "") > 0){
    max.depth <- max(as.numeric(nr_nt), na.rm = TRUE) - 1
  } else {
    max.depth <- 0
  }
  # Check if given grammar_table has the same dimension as given
  # by the create_grammar_table function
  synth_table <- create_grammar_table(Grammar, max.depth = max.depth)
  if(sum(dim(synth_table) != dim(grammar_table)) > 0){
    stop(paste0("The applied search table has the wrong dimensions.
                Try to specify the correct max.depth from the create_grammar_table function.
                In case this error still occures, check if the given search table was really
             produced by create_grammar_table()."))
  }

  # get urGrammar
  urGrammar <- .urGrammar(Grammar, max.depth = max.depth)
  # create list from search table
  tab <- split(grammar_table, grammar_table$non.terminal)
  # reduce list to only include integer vectors
  tab <- lapply(tab, function(x) x[,2])
  # compare number of possible options and the max chosen option for each non-terminal
  # in case the max is larger than the possible number of options -> error
  args_in_tab <- unlist(lapply(tab, max))
  if(sum(is.na(args_in_tab)) > 0) stop("Please provide a search table without NA values.")

  args_in_grammar <- unlist(lapply(urGrammar, length))
  args_in_grammar <- args_in_grammar[names(args_in_grammar) %in% names(args_in_tab)]
  args_in_tab <- data.frame(name = names(args_in_tab), rep = args_in_tab)
  args_in_grammar <- data.frame(name = names(args_in_grammar), rep = args_in_grammar)
  args_grammar_tab <- merge(args_in_grammar, args_in_tab, by = "name", suffixes = c(".grammar", ".tab"))

  if(sum(args_grammar_tab$rep.grammar < args_grammar_tab$rep.tab) > 0){
    stop("There is at least one value in the search table, that is
         larger then the numbers of options for that non-terminal!\n
         e.g. Grammar:      <a> = <b> | <c>
              search table: <a> = 3")
  }

  # define nonterminals
  nonterm <- names(Grammar)
  # Initial Grammar subsetting
  rgram <- as.character(urGrammar[[1]][tab[[nonterm[1]]][1]])
  # delete first tab value
  tab[[nonterm[1]]]<- tab[[nonterm[1]]][-1]
  # While loop until rgram does not includes anymore non-terminal variables
  while(grepl("<(.*?)>", rgram)){
    # get non-terminals in current rgram
    nt_in_rgram <- unlist(regmatches(rgram, gregexpr("<(.*?)>", rgram)))
    # find out if any are at the max.depth length
    terminals_ind <- which(as.numeric(gsub("[^0-9]","",nt_in_rgram)) > max.depth)
    # if there are terminals, split them from the non-terminals
    if(length(terminals_ind) > 0){
      t_in_rgram <- nt_in_rgram[terminals_ind]
      nt_in_rgram <- nt_in_rgram[-terminals_ind]
      for(j in seq_along(t_in_rgram)){
        # if the terminal is in urGrammar than take one of those and put NAs instead of nts
        if(t_in_rgram[j] %in% names(tab)){
          # get Grammar value chosen in tab
          t_value <- urGrammar[[t_in_rgram[j]]][tab[[t_in_rgram[j]]][1]]
          # delete used tab value
          tab[[t_in_rgram[j]]] <- tab[[t_in_rgram[j]]][-1]
          # change all remaining nts to NA
          t_value <- gsub("<.*?>", "NA", t_value)
          # put it in
          rgram <- gsub(t_in_rgram[j], t_value, rgram)
        } else {
          # get Grammar value
          t_value <- urGrammar[[t_in_rgram[j]]]
          # change all remaining nts to NA
          t_value <- gsub("<.*?>", "NA", t_value)
          # put it in
          rgram <- gsub(t_in_rgram[j], t_value, rgram)

        }
      }
      if(length(nt_in_rgram) == 0) next
    }

    # get replacements for the non-terminals
    # tab value:
    tab_val <- lapply(tab[nt_in_rgram], function(x) x[1])
    # delete already used tab values -> only the first will ever be used
    tab[nt_in_rgram] <- lapply(tab[nt_in_rgram], function(x) x[-1])
    # add non-terminal name to value list -> makes next step easier
    for(i in 1:length(tab_val)) tab_val[[i]] <- data.frame(nt = names(tab_val)[i], val = tab_val[[i]],
                                                           stringsAsFactors = FALSE)
    # get replacement
    nt_replacement <- lapply(tab_val, function(x) urGrammar[[x$nt]][x$val])
    # give replacement terminals a unique name, so that they are changed only once
    for(j in 1:length(nt_replacement)) nt_replacement[[j]] <- gsub(">", "*>", nt_replacement[[j]])
    # replace non-terminals with replacement
    for(i in seq_along(nt_in_rgram)){
      rgram <- sub(nt_in_rgram[i], nt_replacement[[i]], rgram)
    }
    # remove unique markers
    rgram <- gsub("\\*>", ">", rgram)
    # end of while loop
  }
  rgram <- .na_remover(rgram)
  return(rgram)
}




