stats.point.features = function(data_frame_feats, list_traces, dataset_type, subject_codes_traces,
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
  subject_codes = data_frame_feats$`Subject code`[master_indices_out]
  
  # Compute the stats
  # df_trim_stats = statistical.test.wrapper(df_trim, parameters,
  #                                         parameters_stats = parameters[['stats']],
  #                                         subject_codes)
  
  # Export the used data
  if (parameters[['handpick_subjects']][['Flag']]) {
    filename_path = paste0(parameters[['name']], '_handpicked.csv')
  } else {
    if (!parameters[['flags']][['skip_age_match']]) {
      filename_path = paste0(parameters[['name']], '_ageMatched.csv')  
    } else {
      filename_path = paste0(parameters[['name']], '_non_ageMatched.csv')  
    }
  }
  
  combine_pathology = TRUE
  if (combine_pathology) {
    cat('\nCOMBINE THE PATHOLOGIES before writing to disk)\n')
    selected = c('CONTROL', 'NTG', 'POAG', 'GLAUCOMA+', 'OTHER GLAUCOMA', 'PACG')
    grouping_vars_out = df_trim[[grouping_variable]]
    factors_in = combine.pathologies(factors_in = grouping_vars_out, factors_kept = selected)
    df_trim[[grouping_variable]] = factors_in
  }
  
  df_trim_export = df_trim
  df_trim_export[['Code']] = subject_codes
  names_to_keep = !grepl('Uncertainty', colnames(df_trim_export)) # remove uncertainties
  df_trim_export = df_trim_export[names_to_keep]
  
  export.pupil.dataframe.toDisk(df_trim_export, filename_path, data_path_out, 'feat_stats')
  
  # traces 
  traces_out = list_traces$pupil[,master_indices_out]
  colnames(traces_out) =  subject_codes
  file_out = gsub('.csv', '_traces.csv', filename_path)
  path_out = file.path(data_path_out, file_out, fsep = .Platform$file.sep)
  write.table(traces_out, file = path_out, sep = ",",  row.names = FALSE) # , col.names = N
  
  # time, same for all
  time_vector_out = list_traces$time_onsetZero[,1]
  file_out = gsub('.csv', '_time_vector.csv', filename_path)
  path_out = file.path(data_path_out, file_out, fsep = .Platform$file.sep)
  write.table(time_vector_out, file = path_out, sep = ",",  row.names = FALSE, col.names = 'time_onsetZero') # , col.names = N
 
  # DEBUG POINT 3b
  save.image(file = file.path(debug_path, 'debug_point3b.Rdata')) # 158.6 MB
  # load(file = file.path(debug_path, 'debug_point3b.Rdata')) # 153.8 MB
  
  # Plot finally 
  if (identical(plot_type, 'boxplot')) {
    p = boxplot.the.features(df_trim, df_trim_stats = df_trim, 
                             feats_to_keep = parameters[['features']], 
                             grouping_variable, parameters, settings)
  } else {
    warning('Only boxplot now implemented, you tried =', plot_type)
  }
  
  if (parameters[['handpick_subjects']][['Flag']]) {
    selected = c('CONTROL', 'GLAUCOMA')
  } else {
    selected = NA
  }
  
  # TODO! Duplicates
  density.and.ROC.plot(df_trim, df_trim_stats, 
                       features = parameters[['features']], var_name_to_plot = 'mean',
                       grouping_variable, combine_pathology = FALSE, 
                       select_groups = selected,
                       parameters, settings)
  
  # if (parameters[['ROC']][['combine_pathologies_also']]) {
  #   parameters[['factors_keep']][[parameters[['main_factor']]]] = c('CONTROL', 'NTG', 'POAG', 'GLAUCOMA+', 'OTHER GLAUCOMA', 'PACG')
  #   density.and.ROC.plot(df_trim, df_trim_stats, 
  #                        features = parameters[['features']], var_name_to_plot = 'mean',
  #                        grouping_variable, combine_pathology = TRUE, parameters, settings)
  # }
  
  # VIDEO Demonstration of stat
  # TODO! Prob does not work really
  # video.demo.of.groups(df_trim, df_trim_stats, list_traces,
  #                      features = parameters[['features']], var_name_to_plot = 'mean',
  #                      master_indices_out, grouping_variable, 
  #                      combine_pathology = TRUE, parameters, settings)
  
  
  
}
