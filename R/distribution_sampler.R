distribution_sampler <- function(functions, variable_df, scaling_bounds = NULL){
  scaling_bounds <- list("slope" = c(0, 90),
                         "aspect" = c(0, 360),
                         "bd" = c(0, NA),
                         "sand" = c(0, 100),
                         "clay" = c(0, 100))
  # check for NAs
  if(sum(is.na(variable_df)) != 0) {
    stop(paste("NA values detected in variable_df!",
         "\nPlease provide a data frame with either imputated NA values",
         "or remove rows with NAs."))
  }
  # scale variables
  add_bounds <- names(variable_df)[!(names(variable_df) %in% names(scaling_bounds))]
  for(variable in add_bounds){
    scaling_bounds[[variable]] <- c(min(variable_df[, variable]),
                                    max(variable_df[, variable]))
  }
  for(variable in names(variable_df)){
      variable_df[, variable] <- .range01(variable_df[, variable],
                                          scaling_bounds[[variable]], na.rm = TRUE)
  }


head(functions, 1000)
}


string <- functions[10, 1]
distribution_values_from_fun(string, variable_df)
