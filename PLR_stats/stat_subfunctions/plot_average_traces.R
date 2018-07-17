plot.average.traces = function(data_frame_feats, list_traces, subject_codes_traces, 
                               dataset_type, derived_feats_names, 
                               parameters, settings) {
  
  # Pre-process, i.e. wrangle the data
  return_list = pre.process.wrapper.for.average.traces(data_frame_feats, list_traces, subject_codes_traces,
                                         parameters, settings, save_matrices_for_deep = TRUE)
    stats_df_out = return_list[[1]]
    scalar_stats = return_list[[2]]
    master_indices_out = return_list[[3]]
  
  # PLOT
  plot_list_of_subset_traces(stats_df_out, scalar_stats,  
                             master_indices_out, grouping_vars_out,
                             data_frame_feats, dataset_type, 
                             parameters, settings)
  
  return(master_indices_out)
  
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
  
  # TODO! Add an option for which 
  
  p = ggplot(stats_df_out %>% filter(variable == 'pupil'), aes(time, value, fill = group)) +
              geom_line(aes(colour = group), size=1)  # means
              # geom_ribbon(aes(ymin=lo, ymax=hi), linetype=2, alpha=0.15) # errors
  
  print(p)
  
  # TODO! add n's, e.g.
  # see example from density.plot.subfunction()
  
}


