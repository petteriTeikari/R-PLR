video.demo.of.groups = function(df_trim, df_trim_stats, list_traces,
                                 features, var_name_to_plot = 'mean',
                                 grouping_variable, master_indices_out,
                                 combine_pathology = TRUE, 
                                 parameters, settings) {
  

  # INITIALIZE --------------------------------------------------------------
  
    library(ggpubr)
    library(cowplot)
    library(ggplot2)
    theme_set(theme_minimal())
  
    source('~/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR/plot/report_visualization.R')
  
    # Go through every feature in the df_trim and plot the distribution of it 
    vars_to_plot = colnames(df_trim)
    vars_to_plot[(vars_to_plot %in% parameters[['main_factor']])] = NA
    factors_in = toupper(df_trim[[grouping_variable]])
    
    # trim the traces
    names_list = names(list_traces)
    for (i in 1 : length(names_list)) {
      list_traces[[names_list[i]]] = list_traces[[names_list[i]]][,master_indices_out]
    }
    
    factors_kept = parameters[['factors_keep']][[parameters[['main_factor']]]]
    if (combine_pathology) {
      factors_in = combine.pathologies(factors_in, factors_kept)
    }
    
    grouping = df_trim[[grouping_variable]]
    group_indices = toupper(grouping) %in% 'CONTROL'
    control_sum = sum(group_indices)
    pathology_sum = sum(!group_indices)
    length_of_pairs = min(c(control_sum, pathology_sum))
    
  # PARAMETERS --------------------------------------------------------------

    transition_speed = 10 
    
  # SPLIT INTO TWO --------------------------------------------------------------
    
    unique_diagnoses = unique(factors_in)
    
    control_out = get.group(list_traces, df_trim, df_trim_stats, group = unique_diagnoses[[1]], 
                            grouping_variable, grouping, length_of_pairs)
      control_traces = control_out[[1]]
      control_feats = control_out[[2]]
    
    pathology_out = get.group(list_traces, df_trim, df_trim_stats, group = unique_diagnoses[[2]], 
                            grouping_variable, grouping, length_of_pairs)
      pathology_traces = pathology_out[[1]]
      pathology_feats = pathology_out[[2]]
    
  # PLOT--------------------------------------------------------------
    
    pupil_cols = c('pupil_raw', 'pupil_outlier_corrected', 'missForest', 'denoised')
    title_strings = c('Raw', 'Outlier free', 'Imputed', 'Denoised')
      
    no_of_subjects = length_of_pairs
    no_of_samples = dim(list_traces$time)[1]
    
    plot_param = list()
    # plot_param[['light_on']] = define.whenLightWasOn(df_with_RGB)
    
    control_traces_stat = matrix(, nrow = no_of_subjects, ncol = no_of_samples)
    control_stat = list()
    pathology_traces_stat = matrix(, nrow = no_of_subjects, ncol = no_of_samples)
    
    for (frame in 1 : length(control_traces)) {
    
      for (subframe in 2 : length(pupil_cols)) {
        
        for (tr in 1 : transition_speed) {
        
          ratio1 = 1 - (tr-1)*(1/transition_speed)
          ratio2 = 1 - ratio1
          
          indices = vector(, length = dim(control_traces$time)[2])
          indices[frame] = TRUE
          
          control_trace = trim.list(list_in = control_traces, 
                                    indices = indices)
          
          # pathology_trace = trim.list(list_in = pathology_traces, 
          #                           indices = indices)
          # 
          progress = as.logical(round(tr / transition_speed))
          if (progress) {
            title_str = title_strings[subframe]
          } else {
            title_str = title_strings[subframe-1]
          }
      
          control_p = plot.trace.visualization(data = control_trace, 
                                               x_col = 'time_onsetZero',
                                               y_cols = pupil_cols[subframe-1],
                                               y_col2 = pupil_cols[subframe],
                                               title_str = title_str, 
                                               xlab_str = 'Time', 
                                               ylab_str = 'Pupil size (normalized)',
                                               ylims = c(-80,20),
                                               plot_type = 'PLR_video',
                                               plot_param,
                                               data_format = 'list',
                                               ratios = c(ratio1, ratio2))
          
          print(control_p)
          
        }
      }
      
      # Add all the traces into a matrix
      trace_to_add = control_trace[[pupil_cols[subframe]]]
      control_traces_stat[frame,] = trace_to_add
      
      # Compute the mean and SD
      mean = colMeans(control_traces_stat, na.rm = TRUE)
      SD = apply(control_traces_stat,2,sd,na.rm=TRUE)
      n = sum(rowSums(is.na(control_traces_stat)) == 0)
      
      control_stat[['time']] = control_trace[['time_onsetZero']]
      control_stat[['mean']] = mean
      control_stat[['SD']] = SD
      
      control_stat_p = plot.trace.visualization(data = control_stat, 
                                                 x_col = 'time',
                                                 y_cols = 'mean',
                                                 y_col2 = NA,
                                                 title_str = paste0(unique_diagnoses[[1]], ' Average, n =', n), 
                                                 xlab_str = 'Time', 
                                                 ylab_str = 'Pupil size (normalized)',
                                                 ylims = c(-80,20),
                                                 plot_type = 'PLR_video',
                                                 plot_param,
                                                 data_format = 'list')
      
      print(control_stat_p)

    }
    
}
    
trim.list = function(list_in, indices) {
  
  names_list = names(list_in)
  for (i in 1 : length(names_list)) {
    list_in[[names_list[i]]] = list_in[[names_list[i]]][,indices]
  }
  
  return(list_in)
  
}

get.group = function(list_traces, df_trim, df_trim_stats, group, grouping_variable,
                     grouping, length_of_pairs) {
  

  group_indices = toupper(grouping) %in% toupper(group)
  
  # shuffle randomly
  indices = which(group_indices)
  rand = sample(indices)
  indices_shuffled = indices[rand]
  
  # and trim to maximum lengths
  indices_shuffled = indices_shuffled[1:length_of_pairs]
  list_traces = trim.list(list_traces, indices_shuffled )
  
  
  df_trim = df_trim[indices_shuffled,]
  # df_trim_stats = df_trim_stats[indices_shuffled,]
  
  return(list(list_traces, df_trim))
  
}
