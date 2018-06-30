pre.process.wrapper.for.point.features = function(data_frame_feats, 
                                                  master_indices_out,
                                                  derived_feats_names,
                                                  parameters, settings, 
                                                  data_type = 'feats') {
  
  cat('Pre-processing the data frame containing ALL THE SUBJECTS and FEATURES for desired variables\n\n')
  fixed_variables = c(parameters[['factors']]) # e.g. Age, Subject Code, ?
  grouping_variable = parameters[['factors']]
  feats_to_keep = parameters[['features']]
  
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
  
  
  # Convert the long format (melt) for plotting
  # df_melt = convert.to.long.melt.format(df_trim, parameters[['factors']]) 
  
  return(df_trim)
  
}