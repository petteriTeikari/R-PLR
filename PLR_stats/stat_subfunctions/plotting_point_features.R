boxplot.the.features = function(df_trim, df_trim_stats,
                                feats_to_keep, 
                                grouping_variable, 
                                parameters, settings) {
  
  # get number of different features, assume that red and blue go side-by-side
  # add a new condition for the "define_by" if you want some other grouping
  define_by = 'features'
  out = get.number.of.diff.features.for.plot(df_trim, define_by, 
                                             grouping_variable)
  
  # TODO!
  # add these: df_trim_stats
  feats_value = out[[1]]
  no_of_subplots = out[[2]]
  no_of_plot_cols = 3 # TODO! if you want this more adaptive
  no_of_plot_rows = ceiling(no_of_subplots / no_of_plot_cols)
  
  p = list()
  for (i in 1:no_of_subplots) {
    
    feat_to_plot = sub('red_', '', feats_value[i])
    melted = trim.to.melt.for.the.feat(df = df_trim, 
                                       feat_to_plot, 
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


# SUBFUNCTIONS ------------------------------------------------------------
# ====
# ====

boxplot.subplot = function(df_values, errors) {
  
  # TODO! add n's, e.g.
  # see example from density.plot.subfunction()
  
  # Plot the features
  ggplot(df_values, aes(x=variable, y=value)) +
    geom_boxplot(aes(fill=factor(Diagnosis))) + 
    labs(title="T", 
         subtitle="t",
         y="% constriction from baseline")
  
}


get.number.of.diff.features.for.plot = function(df_trim, define_by, 
                                                grouping_variable) {
  
  if (identical(define_by, 'features')) {
    
    col_names = colnames(df_trim)
    red_instances_found = lapply(col_names, function(ch) grepl("red", ch))
    # red_indices = lapply(red_instances_found, function(ch) length(ch) != 0)
    red_indices = red_instances_found
    
    # Quick'n'dirty "global" add here
    global_instances_found = lapply(col_names, function(ch) grepl("global", ch))
    
    red_instances = col_names[unlist(red_indices)]
    global_instances = col_names[unlist(global_instances_found)]
    
    red_feats_ind = !(grepl('Uncertainty', red_instances))
    global_feats_ind = !(grepl('Uncertainty', global_instances))
    
    red_feats = red_instances[red_feats_ind]
    no_red_feats = length(red_feats)
    
    global_feats = global_instances[global_feats_ind]
    no_global_feats = length(global_feats)
    
    all_feats = c(red_feats, global_feats)
    no_of_feats = no_red_feats + no_global_feats
    
    return(list(all_feats, no_of_feats))
  }
}

trim.to.melt.for.the.feat = function(df, feat_to_plot, 
                                     grouping_variable) {
  
  col_names = colnames(df)
  
  group_found = match(grouping_variable, col_names)
  if (is.na(group_found)) {
    warning('You would like to group with variable = "', grouping_variable, '" but it was not found from data frame!')
  }
  
  indices_list = find.value.and.uncert.indices(df, feat_to_plot)
  feat_indices = indices_list[[1]]
  uncert_indices = indices_list[[2]]
  
  # combined indices
  indices = feat_indices | uncert_indices
  indices[group_found] = TRUE
  
  # separately 
  feat_indices_w_group = feat_indices
  feat_indices_w_group[group_found] = TRUE
  uncert_indices_w_group = uncert_indices
  uncert_indices_w_group[group_found] = TRUE
  
  df2 <- subset(df, select = feat_indices_w_group)
  
  df_trim_value = df[,feat_indices_w_group]
  df_trim_uncert = df[,uncert_indices_w_group]
  
  errors = combine.uncertainties.for.boxplot(df_trim_value, df_trim_uncert, 
                                             grouping_variable)
  
  df_melt_value = melt(df_trim_value, id.vars=c(grouping_variable))
  df_melt_uncert = melt(df_trim_uncert, id.vars=c(grouping_variable))
  
  return(list(df_melt_value, df_melt_uncert, errors))
  
}

combine.uncertainties.for.boxplot = function(values, uncerts, grouping_variable) {
  
  # values = df_trim_value
  # uncerts = df_trim_uncert
  
  # UNCERTAINTIES
  col_names = colnames(uncerts)
  to_excl = grepl(grouping_variable, col_names)
  
  # now you should have only 2 colors left
  color_names = col_names[!to_excl]
  
  ## TODO!!!
  # include the grouping variable
  ## HERE AS WELL!
  
  # RMS of the uncertainties of all the subjects
  rms = list()
  
  for (col in 1:length(color_names)) {
    
    col_name = color_names[[col]]
    group_vars = unique(uncerts[[grouping_variable]])
    
    for (group in 1 : length(group_vars)) {
      
      group_name = group_vars[[group]]
      
      df_subset_temp = subset(uncerts, uncerts[[grouping_variable]] == group_name)
      vector = df_subset_temp[[col_name]]
      
      squared_sum_uncerts = sum(vector^2)  
      mean_uncerts = squared_sum_uncerts / length(vector)
      
      rms[[group_name]][[col]] = sqrt(mean_uncerts)
      
    }
  }
  
  # VALUES
  
  # Stdev and median of the values
  col_names = colnames(values)
  to_excl = grepl(grouping_variable, col_names)
  
  # now you should have only 2 colors left
  color_names = col_names[!to_excl]
  
  values_stdev = list()
  for (col in 1:length(color_names)) {
    
    col_name = color_names[[col]]
    group_vars = unique(uncerts[[grouping_variable]])
    
    for (group in 1 : length(group_vars)) {
      
      group_name = group_vars[[group]]
      
      df_subset_temp = subset(values, values[[grouping_variable]] == group_name)
      vector = df_subset_temp[[col_name]]
      
      vector_mean = mean(vector, na.rm = TRUE)
      vector_median = median(vector, na.rm = TRUE)
      
      values_stdev[[group_name]][[col]] = sd(vector, na.rm = TRUE)
      
    }
    
  }
  
  # SQUARED COMBINATION
  
  errors = list()
  for (group in 1 : length(group_vars)) {
    group_name = group_vars[[group]]
    errors[[group_name]] = sqrt(rms[[group_name]]^2 + values_stdev[[group_name]]^2)  
  }
  
  return(errors)
}