stats.point.features = function(data_frame_feats, list_traces, dataset_type,
                                master_indices_out, derived_feats_names, plot_type = 'boxplot',
                                parameters, settings) {
  
  # Does the "boring stuff" of getting rid of useless stuff, and wrangling
  # the data to the desired form defined in "parameters"
  df_trim = pre.process.wrapper.for.point.features(data_frame_feats, 
                                                   master_indices_out,
                                                   derived_feats_names,
                                                    parameters, settings, 
                                                    data_type = 'feats') 
  
  grouping_variable = parameters[['main_factor']]
  df_trim_stats = statistical.test.wrapper(df_trim)
  
  
  # Plot finally 
  if (identical(plot_type, 'boxplot')) {
    p = boxplot.the.features(df_trim, df_trim_stats, 
                             feats_to_keep = parameters[['features']], 
                             grouping_variable, parameters, settings)
  } else {
    warning('Only boxplot now implemented, you tried =', plot_type)
  }
  
  # TODO! Duplicates
  density.and.ROC.plot(df_trim, df_trim_stats, 
                       features = parameters[['features']], var_name_to_plot = 'mean',
                       grouping_variable, combine_pathology = FALSE, parameters, settings)
  
  if (parameters[['ROC']][['combine_pathologies_also']]) {
    density.and.ROC.plot(df_trim, df_trim_stats, 
                         features = parameters[['features']], var_name_to_plot = 'mean',
                         grouping_variable, combine_pathology = TRUE, parameters, settings)
  }
  
}