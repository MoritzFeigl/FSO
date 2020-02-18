.grammar_unroller <- function(Grammar, max.depth){
  #### Function to unroll a recursive grammar
  #### inputs: Grammar (list), max.depth (integer)
  #### output: unrolled grammar with unique/non-recursive lines (list)

  # go through all non-terminals and roll the recursive non-terminals out to max depth
  unrolled_grammar <- lapply(1:length(Grammar), .nt_unroller,
                             Grammar = Grammar, max.depth = max.depth)
  unrolled_grammar_unique <- vector(mode = "list")
  # make each recursive non-terminal a named element of the list
  for(k in 1:length(unrolled_grammar)){
    gram_part <- unrolled_grammar[[k]]
    # if grammar part is larger than one row -> split
    if(!is.null(nrow(gram_part))){
      gram_part_list <- vector(mode = "list", length = nrow(gram_part))
      names(gram_part_list) <- rownames(gram_part)
      for (i in 1:nrow(gram_part)) gram_part_list[[i]] <- gram_part[i, ]
      unrolled_grammar_unique <- c(unrolled_grammar_unique, gram_part_list)
    } else {
      unrolled_grammar_unique <- c(unrolled_grammar_unique, unrolled_grammar[k])
    }
  }
  return(unrolled_grammar_unique)
}

.nt_unroller <- function(Grammar, nt_id, max.depth){
  #### Function to unroll a line of a recursive grammar
  #### inputs: Grammar (list), nt_id (integer), max.depth (integer)
  #### output: data frame with unrolled versions of nonterminal (data.frame)

  # get NT name and statements
  nt <- names(Grammar)[nt_id]
  nts <- Grammar[[nt_id]]
  # is recursive?
  if(sum(grepl(nt, nts)) > 0) {
    rnts <- data.frame()
    # Recursive unrolling
    for(i in 1:(max.depth + 1)) rnts <- rbind(rnts, nts, stringsAsFactors = FALSE)
    # Make changed non-terminal at max depth:
    rnts <- rbind(rnts, gsub("<.*?>", "NA", rnts[1, ]), stringsAsFactors = FALSE)

    # check last row and change to usefull arguments
    last_row <- rnts[nrow(rnts), ]
    last_row <- as.character(last_row)
    for(i in seq_along(last_row)){
      tmp <- unlist(strsplit(last_row[i], "+", fixed = TRUE))
      tmp <- unlist(strsplit(tmp, "-", fixed = TRUE))

      if(length(tmp) < 2) {
        tmp <- "NA"
        last_row[i] <- tmp
      }

    }
    rnts[nrow(rnts), ] <- last_row

    # # is a non-terminal in the argument included?
    # nt_incl <- grepl(nt, rnts[max.depth + 1, ])
    # # if non-terminal is included -> remove argument
    # last.row <- as.character(rnts[max.depth + 1, !(nt_incl)])
    #
    # # fill in values, in case that there are some
    # if(length(last.row) > 0){
    #   rnts[nrow(rnts), 1:length(last.row)] <- last.row
    # }
    # col and rownames
    colnames(rnts) <- NULL
    rownames(rnts) <- c(nt, paste0(substr(nt, 1, (nchar(nt)-1) ), 1:(max.depth + 1), ">"))
    # change names corresponding to new non-terminals
    for(i in 1:(nrow(rnts) - 1)){
      rnts[i, ] <- gsub(rownames(rnts)[1], rownames(rnts)[i+1], rnts[i, ])
    }
    return(rnts)
  } else {
    # make a data.frame to define rownames
    nts <- data.frame(t(nts), stringsAsFactors = FALSE)
    colnames(nts) <- NULL
    rownames(nts) <- nt
    return(nts)
  }
}

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

.na_remover <- function(grammar_function){
  ### Function to remove NAs in a function string
  ### inputs: grammar_function (character)
  ### output: function without NAs (character)

  # remove all spaces
  gf <- gsub(" ", "", grammar_function)
  # split everything
  gf_split <- unlist(strsplit(gf, "+"))
  # reassable NA strings
  for(i in seq_along(gf_split[-1])){
    if(gf_split[i] == "N" & gf_split[i+1] == "A") gf_split[i] <- "NA"
  }
  # if there are no more NAs return
  if(sum(gf_split == "NA") == 0) return(paste0(gf_split, collapse = ""))

  # remove As
  gf_split <- gf_split[-(which(gf_split == "NA") + 1)]

  # if there are some NAs just next to each other -> combine into one
  nas <- which(gf_split == "NA")
  while(length(nas) > 1) {
    if(nas[2] == (nas[1] + 1)) {
      gf_split <- gf_split[-nas[1]]
      nas <- nas[-1]
    } else {
      nas <- nas[-1]
    }
  }

  infinite_detector <- 0
  while(sum(gf_split == "NA") != 0){
    # catch infinite recursion
    infinite_detector <- infinite_detector + 1
    if (infinite_detector > length(gf_split)){
      stop("Infinite recursion detected. Check if there are any typos in your Grammar. There might be a terminal adjacent to another without a +, -, *, /")
    }


    # if there is a NA function of the form NA(...) replace the whole thing with NA
    na_ind <- which(gf_split == "NA")
    delete <- matrix(nrow = 1, ncol = 2)

    for (i in na_ind){
      if(i != length(gf_split)){
        # the last NA is left out -> can't be a function
        if(gf_split[i + 1] == "("){
          # find the following ")"
          fclose_ind <- which(gf_split == ")")
          fclose_ind <- fclose_ind[fclose_ind > i][1]
          # position i stays NA and rest will afterwards be deleted
          delete <- rbind(delete, c(i, fclose_ind))
        }
      }
    }
    if(nrow(delete) > 1){
      # delete previously chosen entries
      for(i in nrow(delete):2){
        gf_split <- gf_split[-((delete[i, 1]+1):delete[i, 2])]
      }
    }
    # added or substracted -> NAs are turned to 0
    na_ind <- which(gf_split == "NA")
    for (i in na_ind){
      if(i == 1){
        if(gf_split[i + 1] %in% c("+", "-")) gf_split[i] <- 0
      } else if(i == length(gf_split)){
        if(gf_split[i -1] %in% c("+", "-")) gf_split[i] <- 0
      } else {
        if(gf_split[i - 1] %in% c("+", "-") & gf_split[i + 1] %in% c("+", "-") |
           gf_split[i - 1] %in% c("+", "-") & gf_split[i + 1] == ")" |
           gf_split[i - 1] == "(" & gf_split[i + 1] %in% c("+", "-")
        ){
          gf_split[i] <- 0
        }
      }
    }
    # if there is a * or / -> NAs are turned into 1
    if(sum(gf_split == "NA") == 0) return(paste0(gf_split, collapse = ""))
    na_ind <- which(gf_split == "NA")
    for (i in na_ind){
      if(i == 1){
        if(gf_split[i + 1] %in% c("*", "/")) gf_split[i] <- 1
      } else if(i == length(gf_split)){
        if(gf_split[i -1] %in% c("*", "/")) gf_split[i] <- 1
      } else {
        if(gf_split[i - 1] %in% c("*", "/") | gf_split[i + 1] %in% c("*", "/")){
          gf_split[i] <- 1
        }
      }
    }

    # if NA is in a function -> turn into 1 as well
    if(sum(gf_split == "NA") == 0) return(paste0(gf_split, collapse = ""))
    na_ind <- which(gf_split == "NA")
    for (i in na_ind){
      if(i != 1){
        if(gf_split[i - 1] == "(" & gf_split[i + 1] == ")") gf_split[i] <- 1
      }
    }
  }
  return(paste0(gf_split, collapse = ""))
}

.urGrammar <- function(Grammar, max.depth = NULL){
  ### Function to produce a unrolled grammar
  ### inputs: Grammar (list), max.depth(integer)
  ### output: unrolled grammar (list)

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

  return(urGrammar)
}

.rand_grammar_sampler <- function(Grammar, Grammar_table, max.depth){
  ### Samples functions from a grammar
  ### inputs: Grammar (list), Grammar_table (data frame), max.depth (integer)
  ### output: list with table values and resulting function string (list)
  lens <- unlist(lapply(Grammar, function(x) length(x)))
  names(lens) <- gsub("[<, >]", "", names(lens))
  for(i in 1:nrow(Grammar_table)){
    nt_index <- gsub("[<, >, 0-9]", "", Grammar_table[i, 1]) == names(lens)
    Grammar_table[i, 2] <- sample(lens[nt_index], 1)
  }
  return(list(gram_vector = Grammar_table$value,
              transfer_function =
                generate_function(Grammar, Grammar_table, max.depth = max.depth)
  ))
}

.functions_creator <- function(n, Grammar, Grammar_table, max.depth){
  ### Samples n functions from a grammar
  ### inputs: Grammar (list), Grammar_table (data frame), max.depth (integer)
  ### output: list with table values and resulting function strings (list)
  functions <- vector(mode = "list", length = n)
  for(i in 1:n){
    functions[[i]] <- .rand_grammar_sampler(Grammar = gram,
                                            Grammar_table = Grammar_table,
                                            max.depth = max.depth)
  }
  gram_vectors <- matrix(NA, nrow = nrow(Grammar_table), ncol = n)
  functions_vector <- character(n)
  for(i in 1:n){
    gram_vectors[, i] <- functions[[i]]$gram_vector
    functions_vector[i] <- functions[[i]]$transfer_function
  }
  return(list("gram_vectors" = gram_vectors,
              "functions_vector" = functions_vector))
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
    if(sum(function_variables %in% tf_splitted) == 0) return(NA)
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
