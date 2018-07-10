pre.process.wrapper.for.average.traces = function(data_frame_feats, list_traces, subject_codes_traces,
                                                  parameters, settings) {
  
  # if you select "time_onsetZero" / "pupil_raw" / "error fractional" these are mapped to
  # these standardized names to make further programming easier
  std_naming_of_cols = c('time', 'pupil', 'error')
  
  if (parameters[['handpick_subjects']][['Flag']]) {
    
    # Create a list with the desired data
    out = select.subset.from.list(list_traces, subject_codes_traces, data_frame_feats, 
                                  parameters, settings, 'traces',
                                  std_naming_of_cols, 
                                  handpicked = parameters[['handpick_subjects']][['Flag']])
    
    list_out = out[[1]]
    master_indices_out = out[[2]]
    grouping_vars_out = toupper(out[[3]])
    
    # make this match the following steps
    list_agematched = list_out
    
  } else {
    
    # Create a list with the desired data
    out = select.subset.from.list(list_traces, subject_codes_traces, data_frame_feats, 
                                  parameters, settings, 'traces',
                                  std_naming_of_cols)
    list_out = out[[1]]
    master_indices_out = out[[2]]
    grouping_vars_out = toupper(out[[3]])
    
    age_matched_out = age.match.groups(list_out, master_indices_out, grouping_vars_out,
                                       parameters[['main_factor']], parameters[['match_reference']], 
                                       parameters[['matched_by']][['Age']], data_frame_feats)
    
    list_agematched = age_matched_out[[1]]
    master_indices_out = age_matched_out[[2]]
    
    # Now we might have duplicate column names due to case sensitivities in Master Data Sheet
    grouping_vars_out = age_matched_out[[3]]
    
  }
    
  # Group the list_out based on the grouping variable, e.g. for 4 different
  # pathology groups
  grouped_list = split.into.groups.by.grouping(list_agematched, grouping_vars_out)
  # TODO! You should output this, so this does not have to be repeated again and again?
    
  # Calculate the stats of these groups
  stats_out = calculate.the.stats.of.grouped.lists(grouped_list, std_naming_of_cols)
    vector_stats = stats_out[[1]] # like time, pupil, error
    scalar_stats = stats_out[[2]] # like n
  
  # Make dataframe from list
  # stats_df_out = stats.df.from.list(vector_stats)
  stats_df_out = long.df.stats.from.list(vector_stats)
  
  return(list(stats_df_out, scalar_stats, master_indices_out))
  
}