plot.features.boxplot = function(data_frame_feats, list_traces, dataset_type,
                                 derived_feats_names,
                                 parameters, settings) {
  
  # Create a dataframe with the desired data
  df_subset = select.subset.from.df(data_frame_feats, parameters, settings, 'feats')
  
  # Get rid of the undesired features (if needed for disk export)
  df_dropped = keep.only.the.features.of.interest(df_subset, parameters[['features']], 
                                                  derived_feats_names)
  
  # Do some data wrangling and keep only the desired features for plotting
  fixed_variables = c(parameters[['factors']]) # e.g. Age, Subject Code, ?
  grouping_variable = parameters[['factors']]
  feats_to_keep = parameters[['features']]
  df_trim = trim.df.with.tidyr(df_subset, feats_to_keep, 
                               grouping_variable, fixed_variables) 
  
  # Convert the long format (melt) for plotting
  # df_melt = convert.to.long.melt.format(df_trim, parameters[['factors']]) 
    
  # Plot finally 
  p = boxplot.the.features(df_trim, parameters[['features']], 
                           grouping_variable, parameters, settings)
  
}

boxplot.the.features = function(df_trim, feats_to_keep, 
                                grouping_variable, 
                                parameters, settings) {
  
  # get number of different features, assume that red and blue go side-by-side
  # add a new condition for the "define_by" if you want some other grouping
  define_by = 'features'
  out = get.number.of.diff.features.for.plot(df_trim, define_by, 
                                             grouping_variable)
  feats_value = out[[1]]
  no_of_subplots = out[[2]]
  no_of_plot_cols = 2 # TODO! if you want this more adaptive
  no_of_plot_rows = ceiling(no_of_subplots / no_of_plot_cols)
  
  p = list()
  for (i in 1:no_of_subplots) {
    
    feat_to_plot = sub('red_', '', feats_value[i])
    melted = trim.to.melt.for.the.feat(df_trim, feat_to_plot, 
                                           grouping_variable)
    
    df_values = melted[[1]]
    df_uncert = melted[[2]]
    errors = melted[[3]]
    
    p[[i]] = boxplot.subplot(df_values, errors)
    
  }
  
  # For display
  do.call(grid.arrange, c(p, list(ncol=no_of_plot_cols, nrow=no_of_plot_rows)))
  
  # This for saving for disk
  # https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
  ml <- marrangeGrob(p, ncol=no_of_plot_cols, nrow=no_of_plot_rows)
  
  
}

boxplot.subplot = function(df_values, errors) {
  
  # Plot the features
  ggplot(df_values, aes(x=variable, y=value)) +
    geom_boxplot(aes(fill=factor(Diagnosis))) + 
    labs(title="T", 
         subtitle="t",
         y="% constriction from baseline")
  
}

