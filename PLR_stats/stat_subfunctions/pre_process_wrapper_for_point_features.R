pre.process.wrapper.for.point.features = function(data_frame_feats, 
                                                  master_indices_out,
                                                  derived_feats_names,
                                                  parameters, settings, 
                                                  data_type = 'feats') {
  
  cat('\n')
  cat('Pre-processing the data frame containing ALL THE SUBJECTS and FEATURES for desired variables\n')
  if (length(parameters) == 0) { warning('You have no parameters defined! You need to fix this!') }
  
  fixed_variables = check.that.is.not.null(c(parameters[['factors']]), 'fixed') 
  grouping_variable = check.that.is.not.null(parameters[['factors']], 'fixed') 
  feats_to_keep = check.that.is.not.null(parameters[['features']], 'to_keep') 
  
  # Split bin features and global features
  names_list = split.bin.and.global.feats(derived_feats_names)
  bin_names = names_list[[1]]
  global_names = names_list[[2]]
  
  # Create a dataframe with the desired data, and do age matching also
  # df_subset = select.subset.from.df(data_frame_feats, parameters, settings, data_type)
  df_subset = data_frame_feats[master_indices_out,]
  
  # Do some data wrangling and keep only the desired features for plotting
  df_trim = trim.df.with.tidyr(df_subset, feats_to_keep, bin_names, global_names,
                               grouping_variable, fixed_variables) 
  
  if ((length(df_trim) == 0 | dim(df_trim)[1])) {
    warning('HARD TO CONTINUE WITH THE ANALYSIS from here as no data rows were left after selection criteria')
  }
  
  # Convert the long format (melt) for plotting
  # df_melt = convert.to.long.melt.format(df_trim, parameters[['factors']]) 
  
  return(df_trim)
  
}

check.that.is.not.null = function(var, name) {
  
  if (length(var) == 0) {
    warning('You do not have any variable selected for : "', name, '"!\nThere is a bug somehwere now as this leads to a crash')
    var = NA
  } else {
    return(var)
  }
  
}