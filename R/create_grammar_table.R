#' create_grammar_table
#'
#'Initializes a search table for a given Grammar.
#' @param Grammar
#'
#' @return Empty search table for given grammar
#' @export
#'
#' @examples
#' new_gram <- grammar(a = "<b> + 1, <c>(<b>)",
#'                     b = "1 + <c>(3), 2",
#'                     c = "log, exp, max")
#' search.table(new_gram)
#'
#' recursive_grammar <- grammar(a = "<a> + <b>, <c>(<a>)",
#'                              b = "1 + <c>(3), <b>",
#'                              c = "log, exp")
#' search.table(recursive_grammar, max.depth = 4)
#'
create_grammar_table <- function(Grammar, max.depth = NULL, return_unrolled_grammar = FALSE){
  # remove space from grammar
  Grammar <- lapply(Grammar, function(x) gsub(" ", "", x))
  # Recursive or not?
  nonterm <- names(Grammar)
  rec <- vector(mode ="list", length = length(Grammar))
  for(i in 1:length(Grammar)){
    rec[[i]] <- grep(nonterm[i], Grammar[[i]])
  }
  # Which are the recursive non-terminals
  r_nt <- names(Grammar)[sapply(rec, function(x) length(x) > 0)]

  # Do the recursive non-terminals include other recursive non-terminals?
  # loop over all recursive non-terminals to investigate if they have other rec. nts
  r_nt_pointer <- vector(mode = "list", length = length(r_nt))
  names(r_nt_pointer) <- r_nt
  for(l in seq_along(r_nt)){
    # loop over all recursive non-terminals that are not investigated
    for(n in seq_along(r_nt[-l])) {
      is_rec.nt_inrec.nt <- grepl(r_nt[-l][n], Grammar[r_nt[l]])
      if(is_rec.nt_inrec.nt) r_nt_pointer[[l]] <- c(r_nt_pointer[[l]], r_nt[-l][n])
    }
  }

  # if recursive elements are present
  if(length(r_nt_pointer) > 0){
    # remove non-terminals that have no other recursive non-terminals
    # included in their statement
    r_nt_pointer <- r_nt_pointer[!unlist(lapply(r_nt_pointer, is.null))]
  }

  # Find out if there is one recursive nt that points to more than
  # one recursive nt that points back to it
  r_nt_names <- names(r_nt_pointer)
  finding_deep_recursions <- r_nt_pointer
  for(i in seq_along(r_nt_names)){
    for(j in 1:length(r_nt_pointer[[i]]))
      finding_deep_recursions[[i]][j] <- sum(
        grepl(r_nt_names[i], r_nt_pointer[[r_nt_pointer[[i]][j]]])) > 0
  }
  # if any recursive nt hase more than one recursive nt pointing at him -> error
  for(i in seq_along(r_nt_names)){
    if(sum(as.logical(finding_deep_recursions[[i]])) > 1){
      stop("Recursion width is too deep. At most, only two recursive non-terminals are allowed to refer to each other.")
    }
  }

  # now r_nt_pointer is a named list that includes all recrusive non-terminals that
  # point to another recursive non-terminal. The names are the non-terminals, and the
  # respective list elements are the other rec. non-terminals they point to.

  # if recursive, apply unroll function
  if (length(r_nt) > 0){
    # Recursive search table ->
    if(is.null(max.depth)) {
      stop("Grammar is recursive. Please supply a maximum depth length.")
    }
    # unroll Grammar to include recursive terms, depending on the max.depth
    urGrammar <- .grammar_unroller(Grammar = Grammar, max.depth = max.depth)
  } else {
    urGrammar <- Grammar
  }

  # double recursion:
  # In case there is a double recursion, the higher non-terminal will
  # be set to the value at max.depth.


  # define Grammar hierachy
  gHierachy <- data.frame(nt = names(Grammar), hierachy = 1:length(Grammar))

  # Do only if double recursions exist
  r_nt_double_rec <- vector(mode = "list", length(r_nt_pointer))
  if(length(r_nt_pointer) > 0){
    names(r_nt_double_rec) <- names(r_nt_pointer)
    for (i in 1:length(r_nt_pointer)){
      NTi <- r_nt_pointer[[i]]
      NTi_name <- names(r_nt_pointer)[i]
      # is one of the pointed recursive elements lower in the hierachy (later in Grammar)?
      hierachy_of_nt <- gHierachy[gHierachy$nt == NTi_name, "hierachy"]
      hierachy_of_pointer <- gHierachy[gHierachy$nt %in% NTi, "hierachy"]
      r_nt_double_rec[[i]] <- NTi[hierachy_of_nt > hierachy_of_pointer]
    }
    # remove non-terminals that have no double recursion
    r_nt_double_rec <- r_nt_double_rec[
      unlist(lapply(r_nt_double_rec, function(x) length(x) > 0))
      ]
  }
  # if there are still double recursive elements
  if(length(r_nt_double_rec) > 0){
    # change double recursion links:
    # all that point upwards to a recursion are set to max.depth
    number_of_changes <- numeric()
    for(i in 1:length(r_nt_double_rec)){
      nt_to_change <- names(r_nt_double_rec)[i]
      nt_to_change_into <- r_nt_double_rec[[i]]
      nts_to_change <- names(urGrammar)[grep(nt_to_change,
                                             gsub("[1-9]", "", names(urGrammar))
      )]
      for(k in seq_along(nts_to_change)){
        new_nt_name <- paste0(
          substr(nt_to_change_into, 1, nchar(nt_to_change_into) - 1),
          max.depth + 1, ">")
        urGrammar[[nts_to_change[k]]] <- gsub(nt_to_change_into, new_nt_name,
                                              urGrammar[[nts_to_change[k]]])
      }
      number_of_changes <- c(number_of_changes, length(nts_to_change))
    }
    # produce list with added number of max.depth objects due to double recursion
    for(i in 1:length(r_nt_double_rec)) {
      r_nt_double_rec[[i]] <- cbind(r_nt_double_rec[[i]], number_of_changes[i])
      r_nt_double_rec[[i]] <- cbind(r_nt_double_rec[[i]],
                                    paste0(substr(r_nt_double_rec[[i]][1], 1,
                                                  nchar(r_nt_double_rec[[i]][1]) - 1),
                                           max.depth + 1,
                                           ">"))
    }
  }

  # Double pointing without recursion
  # Find out if there are any nts that point at each other and create a infinite recursion.
  # Don't take in those that are already dealt with in the double recursive part.
  nt_pointer <- vector(mode = "list", length = length(Grammar))
  names(nt_pointer) <- nonterm
  if(length(r_nt_pointer) > 0) {
    nt_pointer <- nt_pointer[!(names(nt_pointer) %in% r_nt_pointer)]
  }
  pointer <- names(nt_pointer)

  # remove nts that have no pointers inside
  ind_empty_point <- logical(length = length(pointer))
  for(l in seq_along(nt_pointer)){
    ind_empty_point[l] <- grepl("<*?>", Grammar[pointer[l]])
  }
  # remove those
  nt_pointer <- nt_pointer[ind_empty_point]
  pointer <- pointer[ind_empty_point]


  for(l in seq_along(nt_pointer)){
    # loop over all non-terminals that are not investigated
    for(n in seq_along(pointer[-l])) {
      points_to <- grepl(pointer[-l][n], Grammar[pointer[l]])
      points_back <- grepl(pointer[l], Grammar[pointer[-l][n]])

      if(sum(points_to, points_back) == 2) nt_pointer[[l]] <- c(nt_pointer[[l]], pointer[-l][n])
    }
  }

  # remove empty nt_pointer
  if(length(nt_pointer) > 0) nt_pointer <- nt_pointer[!unlist(lapply(nt_pointer, is.null))]
  if(length(nt_pointer) > 0){
    # double pointer processing needs a max.depth
    if(is.null(max.depth)) {
      stop("Grammar is recursive. Please supply a maximum depth length.")
    }
  }

  # in case there is more than a double pointing interaction -> stop function
  nt_pointers_larger1 <- unlist(lapply(nt_pointer, function(x) length(x) > 1))
  if(sum(nt_pointers_larger1) > 0){
    stop("Recursion width is too deep. At most, only two non-terminals are allowed to refer to each other.")
  }

  # In case there is a double pointer, the higher non-terminal
  # will be set to the value at max.depth.

  # Do only if double pointer exist
  nt_double_pointer <- vector(mode = "list", length(nt_pointer))
  if(length(nt_pointer) > 0){
    names(nt_double_pointer) <- names(nt_pointer)
    for (i in seq_along(nt_pointer)){
      NTi <- nt_pointer[[i]]
      NTi_name <- names(nt_pointer)[i]
      # is one of the pointed recursive elements lower in the hierachy (later in Grammar)?
      hierachy_of_nt <- gHierachy[gHierachy$nt == NTi_name, "hierachy"]
      hierachy_of_pointer <- gHierachy[gHierachy$nt %in% NTi, "hierachy"]
      nt_double_pointer[[i]] <- NTi[hierachy_of_nt > hierachy_of_pointer]
    }
    # remove non-terminals that are higher in hierachy that the ones they point to
    nt_double_pointer <- nt_double_pointer[unlist(
      lapply(nt_double_pointer, function(x) length(x) > 0)
    )]

    # change double links: all that point upwards to a recursion are set to depth.id
    if(length(nt_double_pointer) > 0){
      number_of_changes <- numeric()
      for(i in 1:length(nt_double_pointer)){
        nt_to_change <- names(nt_double_pointer)[i]
        nt_to_change_into <- nt_double_pointer[[i]]
        nts_to_change <- names(urGrammar)[grep(nt_to_change, gsub("[1-9]", "", names(urGrammar)))]
        # get highest number of current Grammar for nts_to_change_into
        search_nt_to_change_into <- substr(nt_to_change_into, 1, nchar(nt_to_change_into) - 1)
        max.change <- names(urGrammar)[grep(search_nt_to_change_into, names(urGrammar))]
        max.change <- gsub(search_nt_to_change_into, "", max.change)
        max.change <- gsub(">", "", max.change)
        if(sum(max.change %in% "") != length(max.change)){
          depth.id <- max(as.numeric(max.change), na.rm = TRUE)
        } else depth.id <- 1

        for(k in seq_along(nts_to_change)){
          new_nt_name <- paste0(substr(nt_to_change_into, 1, nchar(nt_to_change_into) - 1), depth.id, ">")
          urGrammar[[nts_to_change[k]]] <- gsub(nt_to_change_into, new_nt_name, urGrammar[[nts_to_change[k]]])
        }
        number_of_changes <- c(number_of_changes, length(nts_to_change))
      }
      # produce list with added number of max.depth objects due to double recursion
      for(i in 1:length(nt_double_pointer)) {
        nt_double_pointer[[i]] <- cbind(nt_double_pointer[[i]], number_of_changes[i])
        nt_double_pointer[[i]] <- cbind(nt_double_pointer[[i]],
                                        paste0(substr(nt_double_pointer[[i]][1], 1,
                                                      nchar(nt_double_pointer[[i]][1]) - 1),
                                               depth.id,
                                               ">"))
      }

      # Add new terms max.depth terms to Grammar
      nt_double_pointer_df <- do.call(rbind, nt_double_pointer)
      for(i in 1:nrow(nt_double_pointer_df)){
        new.nt <- vector(mode = "list", length = 1)
        names(new.nt) <- nt_double_pointer_df[i, 3]
        # add only if it is not already existing in unrolled grammar
        if(!(names(new.nt) %in% names(urGrammar))){
          new.nt[[1]] <- urGrammar[[nt_double_pointer_df[i, 1]]]
          # remove all nts
          new.nt[[1]] <- gsub(names(nt_double_pointer)[i], "NA", new.nt[[1]])
          # add new max.depth nt directly after the originial nt
          original_id <- which(names(urGrammar) == nt_double_pointer_df[i, 1])
          urGrammar <- c(urGrammar[1:original_id], new.nt, urGrammar[(original_id + 1):length(urGrammar)])
        }
      }
    }
  }

  # initialise search table lenghts
  sear_dim <- data.frame("non-terminal" = names(urGrammar),
                         "rep" = 1,
                         stringsAsFactors = FALSE)

  #find all <> values and produce table with frequency
  sear_freq <- table(unlist(
    lapply(urGrammar,
           function(x) {
             # Take unique values, only in case that a nt option has a nt occuring
             # more than once take that number.
             if(is.data.frame(x)) x <- as.character(x[1,])
             uGs <- unique(unlist(regmatches(x, gregexpr("<(.*?)>", x))))
             uGs_table <- table(uGs)
             uGa <- regmatches(x, gregexpr("<(.*?)>", x))
             uGa_table <- unlist(lapply(uGa, table))
             uGa_names <- names(uGa_table)
             for(name in unique(uGa_names)){
               uGs_table[names(uGs_table) == name] <- max(uGa_table[uGa_names == name],
                                                          uGs_table[names(uGs_table) == name])
             }
             return(rep(uGs, uGs_table))
           }
    )
  ))

  # update search table length with frequency
  tryCatch({
    for(i in seq_along(sear_freq)){
      ntfrq_id <- sear_dim$non.terminal == names(sear_freq)[i]
      sear_dim$rep[ntfrq_id] <- sear_freq[i]
    }
  }, error = function(err) {
    stop("Error: Number of defined non-terminals and number of used non-terminals in grammar is not the same! Check if there are any typos in your Grammar",
         call. = FALSE)
  })

  ###############VIELLEICHT HIER UPDATE DER VERWIESENEN NTS
  # check if there is one refered nt in the grammar that has less repetition then
  # the one refering to it -> update
  for(i in seq_along(urGrammar)){
    current_nt <- names(urGrammar)[i]
    refered_nts <- unique(unlist(regmatches(as.character(urGrammar[[i]]),
                                            gregexpr("<(.*?)>", urGrammar[[i]]))))
    for(k in seq_along(refered_nts)){
      value_current <- sear_dim$rep[sear_dim$non.terminal == current_nt]
      value_refered <- sear_dim$rep[sear_dim$non.terminal == refered_nts[k]]
      #print(value_current)
      #print(value_refered)
      if(value_current > value_refered) {
        sear_dim$rep[sear_dim$non.terminal == refered_nts[k]] <- value_current
      }
    }

  }

  # Recursive repetitions of non-terminals need to have at least the
  # same amount of repetitions as the original.
  if(length(r_nt) > 0){

    # for double recursion
    if(length(r_nt_double_rec) > 0){
      r_nt_double_rec <- do.call(rbind, r_nt_double_rec)
      r_nt_double_rec <- data.frame(nt = r_nt_double_rec[, 3], rep = as.numeric(r_nt_double_rec[, 2]),
                                    stringsAsFactors = FALSE)
      r_nt_double_rec <- r_nt_double_rec[r_nt_double_rec$nt == unique(r_nt_double_rec$nt), ]
      changed_sear_dim <- sear_dim$non.terminal %in% r_nt_double_rec$nt
      sear_dim$rep[changed_sear_dim] <-  sear_dim$rep[changed_sear_dim] - r_nt_double_rec$rep
    }

    for (i in seq_along(r_nt)){
      nt_chain_ind <- grep(pattern = substr(r_nt[i], 1, nchar(r_nt[i]) - 1),
                           x = sear_dim$non.terminal)
      # check hierarchy order of chain id
      chain_id_numbers <- gsub(pattern = substr(r_nt[i], 1, nchar(r_nt[i]) - 1),
           replacement = "",
            sear_dim$non.terminal[nt_chain_ind]) %>%
        gsub(">", "", .)
      chain_id_numbers[chain_id_numbers == ""] <- "0"
      nt_chain_ind <- nt_chain_ind[order(chain_id_numbers)]

      for(k in 1:(length(nt_chain_ind) - 1)){
        if(sear_dim$rep[nt_chain_ind[k]] > sear_dim$rep[nt_chain_ind[k + 1]]){
          sear_dim$rep[nt_chain_ind[k]] <- sear_dim$rep[nt_chain_ind[k + 1]]
        }
      }
    }

    # add double recursion values again
    if(length(r_nt_double_rec) > 0){
      sear_dim$rep[changed_sear_dim] <-  sear_dim$rep[changed_sear_dim] + r_nt_double_rec$rep
    }
  }

  # Double pointing repetitions of non-terminals need to have the same amount
  # of repetitions as the original
  if(length(nt_double_pointer) > 0){
    for(i in 1:length(nt_double_pointer)){
      end_point <- names(nt_double_pointer)[i]
      maxdepth_start <- nt_double_pointer[[i]][, 3]
      new_rep <- sear_dim$rep[sear_dim$non.terminal == nt_double_pointer[[i]][, 1]]
      # in case that a nt is occuring more often in a single option, new_rep can be larger
      new_rep <- max(new_rep, sear_dim$rep[sear_dim$non.terminal %in% c(end_point, maxdepth_start)])
      sear_dim$rep[sear_dim$non.terminal %in% c(end_point, maxdepth_start)] <- new_rep
    }
  }

  # Recursive repetitions of non-terminals which are also pointed to have the added
  # amount of repetitions!

  # check if there is a nt that is both recursive and the upper nt of a double pointer
  # is a single check because only one double pointer is allowed! recursion width restr.
  if(length(nt_double_pointer) > 0){
    if(nt_double_pointer[[1]][, 1] %in% r_nt) {
      pointer <- names(nt_double_pointer)
      point <- nt_double_pointer[[1]][, 3]
      sear_dim$rep[sear_dim$non.terminal == point] <- sear_dim$rep[
        sear_dim$non.terminal == point] +
        sear_dim$rep[sear_dim$non.terminal == pointer]
    }
  }


  # unfold table
  search_table <- data.frame("non-terminal" = rep(sear_dim$non.terminal, sear_dim$rep),
                             "value" = NA,
                             stringsAsFactors = FALSE)
  # in case that max.depth nts have only one option -> remove them, otherwise let them be

  # get num index of nts
  nr_nt <- gsub("[^0-9]", "", search_table$non.terminal)
  # which are max.depth nts
  rm_nt <- which(as.numeric(nr_nt) > max.depth)
  # Which terminals have only one option
  option_l1 <- logical(length(rm_nt))
  for(i in seq_along(rm_nt)){
    option_l1[i] <- length(unlist(urGrammar[search_table$non.terminal[rm_nt[i]]])) == 1
  }
  # remove those with only one option from the search table
  rm_nt <- rm_nt[option_l1]
  if(length(rm_nt) > 0){
    search_table <- search_table[-rm_nt, ]
  }
  row.names(search_table) <- NULL
  if(return_unrolled_grammar) return(urGrammar)
  return(search_table)
}


