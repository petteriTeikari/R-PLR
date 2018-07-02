import.feat.file = function(file_in, color) {
  
  # Import the file
  df_file_import <- read.table(file_in, header = TRUE, sep=',', stringsAsFactors=FALSE)
  # df_file_import <- read.csv(file_in, header = TRUE, sep=',', stringsAsFactors=FALSE)
  
  # Do not save the "metadata" (i.e. the vars_to_excl) to the "factor dataframe
  vars_to_excl = c("Method", "Start", "End", "StartString", "Description", "Domain")
  '%ni%' <- Negate('%in%') # define the inverse of %in%
  df_file_in = df_file_import[ , (names(df_file_import) %ni% vars_to_excl)]
  
  # Now we have data frame with the colnames as variables, and "Value" is a 
  # variable containing as many rows as there are features, shape the dataframe
  new_feats_per_file = reshape.dataframeRowsToCols(df_file_in, color)
  
  derived_feats_names_blue = NA
  derived_feats_names_red = NA
  derived_feats_names_global = NA
  
  # return the names for the features computed from the traces
  if (identical(color, 'blue')) {
    derived_feats_names_blue = colnames(new_feats_per_file)
  } else if (identical(color, 'red')) {
    derived_feats_names_red = colnames(new_feats_per_file)
    
    # Quick'n'dirty fix for plotting
    # TODO! You could keep global totally separate
  } else if (identical(color, 'global')) {
    derived_feats_names_global = colnames(new_feats_per_file)
  }
  
  return(list(new_feats_per_file, derived_feats_names_blue, derived_feats_names_red, derived_feats_names_global))
  
}

reshape.dataframeRowsToCols = function(df_file_in, color) {
  
  # Maybe there is an easier way to do this, but now replicate the $Name column
  # to have _value and _uncertainty suffix
  
  # TODO! Check this mess 
  no_of_variables = length(df_file_in$Value)
  
  new_var_names = list()
  for (j in 1:length(df_file_in$Value)) {
    colname = df_file_in$Name[j]
    str_variable_a = paste(color, '_', colname, sep = "")
    new_var_names = c(new_var_names, str_variable_a)
    str_variable_b = paste(color, '_', colname, '_', 'Uncertainty', sep = "")
    new_var_names = c(new_var_names, str_variable_b)
  }
  new_var_names = unlist(new_var_names)
  
  lst <- setNames(vector("list", length(new_var_names)), new_var_names)
  for (j in 1:length(new_var_names)) {
    
    if (j %% 2 != 0) {
      in_ind = (j+1)/2
      lst[[new_var_names[j]]] = df_file_in$Value[in_ind]
    } else {
      in_ind = j/2
      lst[[new_var_names[j]]] = df_file_in$Uncertainty[in_ind]
    }
  } 
  
  # The list converted to a dataframe
  # https://stackoverflow.com/questions/46399784/create-empty-dataframe-and-error-replacement-has-1-row-data-has-0-occurs
  new_feats_per_file = data.frame(lst)
  
  return(new_feats_per_file)
  
}