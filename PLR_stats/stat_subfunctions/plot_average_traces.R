plot.average.traces = function(data_frame_feats, list_traces, subject_codes_traces, 
                               dataset_type, derived_feats_names, 
                               parameters, settings) {
  
  # source(file.path(settings[['stat_path']], 'data_wrangling_functions.R', fsep = .Platform$file.sep))
  
  # Create a list with the desired data
  out = select.subset.from.list(list_traces, subject_codes_traces, data_frame_feats, 
                                        parameters, settings, 'traces')
    list_out = out[[1]]
    master_indices_out = out[[2]]
    grouping_vars_out = out[[3]]
  
  # Group the list_out based on the grouping variable, e.g. for 4 different
  # pathology groups
  grouped_list = split.into.groups.by.grouping(list_out, grouping_vars_out)
    
  # Calculate the stats of these groups
  stats_out = calculate.the.stats.of.grouped.lists(grouped_list)
    vector_stats = stats_out[[1]]
    scalar_stats = stats_out[[2]]
    
  # Make dataframe from list
  # stats_df_out = stats.df.from.list(vector_stats)
  stats_df_out = long.df.stats.from.list(vector_stats)
    
    
  # PLOT
  plot_list_of_subset_traces(stats_df_out, scalar_stats, 
                             master_indices_out, grouping_vars_out,
                             data_frame_feats, dataset_type, 
                             parameters, settings)
  
  
}

# SUBFUNCTION TO PLOT
plot_list_of_subset_traces = function(stats_df_out, scalar_stats, 
                                       master_indices_out, grouping_vars_out,
                                       data_frame_feats, dataset_type, 
                                       parameters, settings) {

  # the melt option would work well without the individual errors
  # https://stackoverflow.com/questions/40093238/r-using-geom-ribbon-with-melted-dataframe
  # https://stackoverflow.com/questions/41225301/r-geom-ribbon-data-transformation

  # BETTER WAY NOW  
  # http://www.evanpickett.com/blog/2015/7/3/organising-data-for-graphs-in-r
  
  p = ggplot(data = stats_df_out, aes(x=time, y=pupil, fill = group)) +
          geom_line(aes(colour = group), size=1) + # means
          geom_ribbon(aes(ymin=lo, ymax=hi), linetype=2, alpha=0.15) # errors
  
  print(p)
  
  # TODO! add n's, e.g.
  str(scalar_stats)
  
}


