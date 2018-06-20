STAT.wrapper = function(data_frame_feats, list_traces, subject_codes_traces, 
                        dataset_type, derived_feats_names, 
                        parameters, settings) {
  
  # INIT Setting -----------------------------------------------------------------
  
    # SOURCE SUBFUNCTIONS used
    source(file.path(settings[['stat_path']], 'plot_average_traces.R', fsep = .Platform$file.sep))
    source(file.path(settings[['stat_path']], 'plot_features_boxplot.R', fsep = .Platform$file.sep))
    
    # "private functions"
    source(file.path(settings[['stat_path']], 'data_wrangling_functions.R', fsep = .Platform$file.sep))
  
  # Init PROCESS Parameters -----------------------------------------------------------------  
  
    # TODO! Empty input as well need to be accepted
  
    # NOTE! The name should be the same as found from the Master Data sheet
  
      # get rid of patients that do not match there, for example useful if 
      # you want to save a reduced data frame to disk
  
      # "PRIMARY VARIABLES", used also for matching if there is any
      parameters[['main_factor']] = 'Diagnosis'
      parameters[['factors_keep']][[parameters[['main_factor']]]] = 
        c('Control', 'POAG', 'PACG', 'NTG', 'DISC SUSPECT')
      
      # Secondary, tertiary, factors if needed
      # parameters[['factors_keep']][['Race']] = c(1)
      
    # If you want to match let's say by age, getting rid of the non-matching ages for example
      
      parameters[['match_reference']] = 'Control' 
      parameters[['matched_by']][['Age']] = 10 # threshold in years
    
    # The factor(s) that you want to use to group the plots,
      
      # In other words, not kicking any data out, just selecting
      # the variables to show on the plot
      parameters[['factors']] = 'Diagnosis'
      
    # The features of interest
      
      parameters[['features']] = c('QuickPhasic', '6SecondPIPR', 'SlopesON_2_phasic', 'SlopesOFF_3_phasic')
    
    # The traces of interest
      
      parameters[['traces']][['x']] = 'time'
      parameters[['traces']][['y']] = 'pupil'
      parameters[['traces']][['error']] = 'error'
    
    # TODO! You could read all the parameter pairs from .txt files, and have one row
    # per comparison and simply then loop through the rows giving you all the desired 
    # stats with a button push
    
      # now the [1] is a placeholder for the 1st row
      analysis_param = list()
      analysis_param[[1]] = parameters
    
  # PROCESS -----------------------------------------------------------------  
    
    no_of_different_analyses = length(analysis_param)
    
    # Now go through all the analyses wanted
    for (analys_ind in 1 : no_of_different_analyses) {
    
      # Plot average traces
      p1 = plot.average.traces(data_frame_feats, list_traces, subject_codes_traces, 
                               dataset_type, derived_feats_names, 
                               analysis_param[[analys_ind]], settings)
      
      # Show the feats then
      p2 = plot.features.boxplot(data_frame_feats, list_traces, dataset_type, 
                                 derived_feats_names,
                                 analysis_param[[analys_ind]], settings)
              
    }
      
}



