video.demo.of.groups = function(df_trim, df_trim_stats, list_traces,
                                 features, var_name_to_plot = 'mean',
                                 grouping_variable, master_indices_out,
                                 combine_pathology = TRUE, 
                                 parameters, settings) {
  

  # INITIALIZE --------------------------------------------------------------
  
    library(ggplot2)
    theme_set(theme_minimal())
  
    source('~/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR/plot/report_visualization.R')
    source('~/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR/scripts/combine_data_from_multiple_folders.R')
  
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
    
    filecodes = data_frame_feats[master_indices_out,]$`Subject code`
    
  # PARAMETERS --------------------------------------------------------------

    transition_speed = 6
    
  # SPLIT INTO TWO --------------------------------------------------------------
    
    unique_diagnoses = unique(factors_in)
    
    control_out = get.group(list_traces, df_trim, df_trim_stats, group = unique_diagnoses[[1]], 
                            grouping_variable, grouping, length_of_pairs)
    
      control_traces = control_out[[1]]
      control_feats = control_out[[2]]
      control_indices_shuffled = control_out[[3]]
    
    pathology_out = get.group(list_traces, df_trim, df_trim_stats, group = factors_kept[2:3], 
                            grouping_variable, grouping, length_of_pairs)
    
      pathology_traces = pathology_out[[1]]
      pathology_feats = pathology_out[[2]]
      pathology_indices_shuffled = pathology_out[[3]]
    
  # PLOT--------------------------------------------------------------
    
    pupil_cols = c('pupil_raw', 'missForest', 'denoised')
    title_strings = c('Raw', 'Imputed', 'Denoised')
      
    no_of_subjects = length_of_pairs
    no_of_samples = dim(list_traces$time)[1]
    
    plot_param = list()
    # plot_param[['light_on']] = define.whenLightWasOn(df_with_RGB)
    
    control_traces_stat = matrix(, nrow = no_of_subjects, ncol = no_of_samples)
    control_stat = list()
    pathology_traces_stat = matrix(, nrow = no_of_subjects, ncol = no_of_samples)
    pathology_stat = list()
    
    counter = 0
    p = list()
        
    for (frame in 1 : dim(control_traces$time)[2]) {

      # TODO! Add processing phases from raw to denoised
      for (subframe in 2 : length(pupil_cols)) {
        
        # TODO! Add actual transitions between processing phases
        for (tr in 1 : transition_speed) {
        
          # list_out = generate_frame(p, frame, subframe, tr, 
          #                           transition_speed, unique_diagnoses, pupil_cols,
          #                           control_traces, pathology_traces, 
          #                           control_traces_stat, control_stat, 
          #                           pathology_traces_stat, pathology_stat)
          
          ratio1 = 1 - (tr-1)*(1/(transition_speed-1))
          ratio2 = 1 - ratio1
          #ratio1 = 0
          #ratio2 = 1
          
          indices = vector(, length = dim(control_traces$time)[2])
          indices[frame] = TRUE
          
          control_trace = trim.list(list_in = control_traces, 
                                    indices = indices)
          
          pathology_trace = trim.list(list_in = pathology_traces, 
                                      indices = indices)
           
          progress = as.logical(round(tr / transition_speed))
          if (progress) {
            title_str = title_strings[subframe]
          } else {
            title_str = title_strings[subframe-1]
          }
      
          p[[1]] = plot.trace.visualization(data = control_trace, 
                                               x_col = 'time_onsetZero',
                                               y_cols = pupil_cols[subframe-1],
                                               y_col2 = pupil_cols[subframe],
                                               title_str = paste(title_str, '|', 
                                                                 unique_diagnoses[[1]], '|',
                                                                 filecodes[control_indices_shuffled[frame]]), # title_str, 
                                               xlab_str = 'Time [s]', 
                                               ylab_str = 'Pupil size',
                                               ylims = c(-80,20),
                                               plot_type = 'PLR_video',
                                               plot_param,
                                               data_format = 'list',
                                               ratios = c(ratio1, ratio2))
          
          p[[2]] = plot.trace.visualization(data = pathology_trace, 
                                               x_col = 'time_onsetZero',
                                               y_cols = pupil_cols[subframe-1],
                                               y_col2 = pupil_cols[subframe],
                                               title_str = paste(title_str, '|', 
                                                                 unique_diagnoses[[2]], '|',
                                                                 filecodes[pathology_indices_shuffled[frame]]), # title_str, 
                                               xlab_str = 'Time [s]', 
                                               ylab_str = 'Pupil size',
                                               ylims = c(-80,20),
                                               plot_type = 'PLR_video',
                                               plot_param,
                                               data_format = 'list',
                                               ratios = c(ratio1, ratio2))
          
          # do.call(grid.arrange, c(p, list(ncol=2, nrow=2)))
          
          counter = counter + 1
          cat(counter, ' ')

          ml <- marrangeGrob(p, nrow=2, ncol=2, list(top=NULL) )

          filename_out = paste0(sprintf("%04d", counter), '.png')
          ggsave(filename_out, ml, width = 6.4, height = 3.6, units = "in", dpi = 300)
          
        }
      }
      
      
      # Add all the traces into a matrix
      trace_to_add = control_trace[[pupil_cols[subframe]]]
      control_traces_stat[frame,] = trace_to_add
      
      # Compute the mean and SD
      mean = colMeans(control_traces_stat, na.rm = TRUE)
      SD = apply(control_traces_stat,2,sd, na.rm=TRUE)
      n = sum(rowSums(is.na(control_traces_stat)) == 0)
      
      control_stat[['time']] = control_trace[['time_onsetZero']]
      control_stat[['mean']] = mean
      control_stat[['SD']] = SD
      
      p[[3]] = plot.trace.visualization(data = control_stat, 
                                                 x_col = 'time',
                                                 y_cols = 'mean',
                                                 y_col2 = NA,
                                                 title_str = paste0(unique_diagnoses[[1]], ' Average, n =', n), 
                                                 SD = SD,
                                                 xlab_str = 'Time [s]', 
                                                 ylab_str = 'Pupil size',
                                                 ylims = c(-80,20),
                                                 plot_type = 'PLR_video',
                                                 plot_param,
                                                 data_format = 'list')
      
      # Add all the traces into a matrix
      trace_to_add = pathology_trace[[pupil_cols[subframe]]]
      pathology_traces_stat[frame,] = trace_to_add
      
      # Compute the mean and SD
      mean = colMeans(pathology_traces_stat, na.rm = TRUE)
      SD = apply(pathology_traces_stat,2,sd, na.rm=TRUE)
      n = sum(rowSums(is.na(pathology_traces_stat)) == 0)
      
      pathology_stat[['time']] = pathology_trace[['time_onsetZero']]
      pathology_stat[['mean']] = mean
      pathology_stat[['SD']] = SD
      
      p[[4]] = plot.trace.visualization(data = pathology_stat, 
                                                x_col = 'time',
                                                y_cols = 'mean',
                                                y_col2 = NA,
                                                title_str = paste0(unique_diagnoses[[2]], ' Average, n =', n), 
                                                SD = SD,
                                                xlab_str = 'Time [s]', 
                                                ylab_str = 'Pupil size',
                                                ylims = c(-80,20),
                                                plot_type = 'PLR_video',
                                                plot_param,
                                                data_format = 'list')
      
      # do.call(grid.arrange, c(p, list(ncol=2, nrow=2)))
      ml <- marrangeGrob(p, nrow=2, ncol=2, list(top=NULL) )
      
      counter = counter + 1
      cat(counter, ' ')
      
      filename_out = paste0(sprintf("%04d", counter), '.png')
      ggsave(filename_out, ml, width = 6.4, height = 3.6, units = "in", dpi = 300)
      # 
      
      
      # TODO! 
      # This does not get updated in a loop actually, check fixing from aes() to aes_q()
      
      # return(list(control_traces_stat, control_stat, pathology_traces_stat, pathology_stat))


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
  rand = sample(1:length(indices))
  indices_shuffled = indices[rand]
  indices_shuffled = indices_shuffled[!is.na(indices_shuffled)]
  
  # and trim to maximum lengths
  indices_shuffled = indices_shuffled[1:length_of_pairs]
  list_traces = trim.list(list_traces, indices_shuffled )
  
  
  df_trim = df_trim[indices_shuffled,]
  # df_trim_stats = df_trim_stats[indices_shuffled,]
  
  return(list(list_traces, df_trim, indices_shuffled))
  
}
