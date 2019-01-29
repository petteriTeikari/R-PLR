pre.process.wrapper.for.average.traces = function(data_frame_feats, list_traces, subject_codes_traces,
                                                  combine_pathology, parameters, settings, save_matrices_for_deep = FALSE) {
  
  # if you select "time_onsetZero" / "pupil_raw" / "error fractional" these are mapped to
  # these standardized names to make further programming easier
  std_naming_of_cols = c('time', 'pupil', 'error')
  
  if (parameters[['handpick_subjects']][['Flag']]) {
    
    warning('Handpicking the subjects now!')
    
    # Create a list with the desired data
    out = select.subset.from.list(list_traces, subject_codes_traces, data_frame_feats, 
                                  parameters, settings, 'traces',
                                  std_naming_of_cols, 
                                  handpicked = parameters[['handpick_subjects']][['Flag']])
    
    list_out = out[[1]]
    master_indices_out = out[[2]]
    grouping_vars_out = toupper(out[[3]])
    subject_codes_out = data_frame_feats$`Subject code`[master_indices_out]
    
    # write traces to disk
    time_to_disk = list_out$time[,1]
    pupil_to_disk = list_out$pupil
    colnames(pupil_to_disk) = subject_codes_out
    error_to_disk = list_out$error
    colnames(error_to_disk) = subject_codes_out
    
    path_out = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/for_deepLearning'
    file_out = file.path(path_out, paste0('pupil_traces_study_n302.csv'), fsep = .Platform$file.sep)
    write.table(cbind(time_to_disk, pupil_to_disk), file = file_out, sep = ",", row.names=FALSE, col.names=TRUE)
    file_out = file.path(path_out, paste0('error_traces_study_n302.csv'), fsep = .Platform$file.sep)
    write.table(cbind(time_to_disk, error_to_disk), file = file_out, sep = ",", row.names=FALSE, col.names=TRUE)
    
    if (parameters[['handpick_subjects']][['Flag']]) {
      
      cat('WE ARE IGNORING THE GROUPING VARIABLE NOW\n')
      cat('i.e. only the hand-picked BOOLEAN flag determines what subjects we keep\n')
      cat('having the files picke by Raymond in other words so that all the analyses are using the same subjects\n')
      selected = c('CONTROL', 'NTG', 'POAG', 'GLAUCOMA+', 'OTHER GLAUCOMA', 'PACG')
      
      # TODO! Why did the BOOLEAN become a CHAR
      if ((as.logical(parameters[['handpick_subjects']][['Flag']]) && 
           as.logical(parameters[['handpick_subjects']][['ignore_diagnosis_column']]))) {
         # TODO, we actually manually defined above
         cat('   manual fix now FOR IGNORING THE DIAGNOSIS COLUMN when HANDPICKING')
       }
      
      selected_ind = grouping_vars_out %in% selected
      master_indices_out = master_indices_out[selected_ind]
      grouping_vars_out = grouping_vars_out[selected_ind]
      
      names_list = names(list_out)
      for (i in 1 : length(names_list)) {
        list_out[[names_list[i]]] = list_out[[names_list[i]]][,selected_ind]
      }
        
      
    } else {
      selected = NA
    }
    
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
    
    if (!parameters[['flags']][['skip_age_match']]) {
    
      age_matched_out = age.match.groups(list_out, master_indices_out, grouping_vars_out,
                                         main_factor = parameters[['main_factor']], 
                                         match_reference = parameters[['match_reference']], 
                                         match_threshold = parameters[['matched_by']][['Age']], 
                                         data_frame_feats)
      
      list_agematched = age_matched_out[[1]]
      master_indices_out = age_matched_out[[2]]
      
      # Now we might have duplicate column names due to case sensitivities in Master Data Sheet
      grouping_vars_out = age_matched_out[[3]]
      
    } else {
      
      cat('\n')
      warning(' Skipping age-matching now!')
      cat('\n')
      list_agematched = list_out
      
    }
    
  }
    
  # Group the list_out based on the grouping variable, e.g. for 4 different
  # pathology groups
  grouped_list = split.into.groups.by.grouping(list_agematched, grouping_vars_out, parameters)
  # TODO! You should output this, so this does not have to be repeated again and again?
    
  if (combine_pathology) {
    cat('\nCOMBINE THE PATHOLOGIES (over-riding the above-listed fine-grained split!)\n')
    factors_in = combine.pathologies(factors_in = grouping_vars_out, factors_kept = selected)
    grouped_list = split.into.groups.by.grouping(list_agematched, factors_in, parameters)
  }
  
  # Calculate the stats of these groups
  stats_out = calculate.the.stats.of.grouped.lists(grouped_list, std_naming_of_cols)
    vector_stats = stats_out[[1]] # like time, pupil, error
    scalar_stats = stats_out[[2]] # like n
  
  # Make dataframe from list
  # stats_df_out = stats.df.from.list(vector_stats)
  stats_df_out = long.df.stats.from.list(vector_stats)
  
  cat('\n')
  if (save_matrices_for_deep) {
    cat('Exporting the picked PLR traces for deep learning\n')
    save.matrices.for.deep.learning(data_frame_feats, list_traces, subject_codes_out,
                                    list_agematched, master_indices_out, grouping_vars_out,
                                    combine_pathology = TRUE, 
                                    factors_kept = selected,
                                    parameters, settings)
  } else {
    cat('! Skipping the data export for deep learning purposes !\n')
  }
  cat('\n')
  
  return(list(stats_df_out, scalar_stats, master_indices_out))
  
}