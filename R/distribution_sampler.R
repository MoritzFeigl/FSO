distribution_sampler <- function(functions, variable_df,
                                 scaling_bounds = NULL,
                                 file_name = NULL,
                                 no_cores = NULL){

  options(scipen = 999)

    # check for NAs
  if(sum(is.na(variable_df)) != 0) {
    stop(paste("NA values detected in variable_df!",
               "\nPlease provide a data frame with either imputated NA values",
               "or remove rows with NAs."))
  }

  if(sum(names(variable_df) %in% names(scaling_bounds)) != ncol(variable_df)){
    stop(paste("Not all variables of variable_df are included in the provided scaling_bounds",
               "\nPlease provide scaling bounds that fit variable_df, or check column names."))
  }
  # scale variables
  add_bounds <- names(variable_df)[!(names(variable_df) %in% names(scaling_bounds))]
  for(variable in add_bounds){
    scaling_bounds[[variable]] <- c(min(variable_df[, variable]),
                                    max(variable_df[, variable]))
  }
  for(variable in names(variable_df)){
    variable_df[, variable] <- .range01(variable_df[, variable],
                                        scaling_bounds[[variable]])
    variable_df[, variable][variable_df[, variable] < 0.01] <- 0.01
  }
  if(is.data.frame(functions)){
    if(ncol(functions) == 1){
      functions <- as.character(functions[, 1])
    } else stop("functions must be either a character vector or a 1 column data frame with function strings.")
  } else {
    if(!is.character(functions)){
      stop("functions must be either a character vector or a 1 column data frame with function strings.")
    }
  }

  # define file_name if not specified
  if(is.null(file_name)){
    file_name <- paste0("variable_input_grammar", "-",
                        format(Sys.time(), "%d-%m-%Y-%H%M"))
  }
  # create folder
  if(!grepl(".fst", file_name)) file_name <- paste0(file_name, ".fst")
  cat("Results will be saved in", file_name)
  #if(!dir.exists(file_name))  dir.create(file_name)



  # parallel computation
  if(Sys.info()[["sysname"]] == "Windows"){
    cl <- parallel::makeCluster(no_cores)
    parallel::clusterExport(cl, list(".evaluate_function_from_string",
                                     ".distribution_values_from_fun",
                                     "variable_df"
    ),
    envir = environment())
    output <- pbapply::pblapply(functions,
                                FUN = .distribution_values_from_fun,
                                variable_df = variable_df,
                                cl = cl)
    parallel::stopCluster(cl)
    output <- do.call(rbind, output)
  } else {
   output <- parallel::mclapply(functions,
                                          FUN = .distribution_values_from_fun,
                                          variable_df = variable_df,
                                          mc.cores = no_cores )
   output <- do.call(rbind, output)
  }

   output <- output[!is.na(output$median), ]
   fst::write_fst(x = output, path = file_name, compress = 100)
   rm(output)
}



 # for(i in 1:1000){
 #
 #  print(functions[i])
 #   ar <- lapply(functions[i],
 #          FUN = .distribution_values_from_fun,
 #          variable_df = variable_df)
 #   if(sum(is.na(ar)) > 0){
 #     print(i)
 #   print(ar)
 #   }
 # }
