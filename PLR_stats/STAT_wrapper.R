STAT.wrapper = function(data_frame_feats, list_traces, subject_codes_traces, 
                        dataset_type, derived_feats_names, 
                        parameters, settings) {
  
  # INIT Setting -----------------------------------------------------------------
  
    # SOURCE SUBFUNCTIONS used
    source(file.path(settings[['stat_path']], 'plot_average_traces.R', fsep = .Platform$file.sep))
  
    source(file.path(settings[['stat_path']], 'stats_point_features.R', fsep = .Platform$file.sep))
    source(file.path(settings[['stat_path']], 'plotting_point_features.R', fsep = .Platform$file.sep))
    source(file.path(settings[['stat_path']], 'statistical_test_wrapper.R', fsep = .Platform$file.sep))
    source(file.path(settings[['stat_path']], 'compute_prelim_stat_tests.R', fsep = .Platform$file.sep))
    
    # "private functions"
    source(file.path(settings[['stat_path']], 'data_wrangling_functions.R', fsep = .Platform$file.sep))
    source(file.path(settings[['stat_path']], 'pre_process_wrapper_for_point_features.R', fsep = .Platform$file.sep))
    source(file.path(settings[['stat_path']], 'pre_process_wrapper_for_average_traces.R', fsep = .Platform$file.sep))
  
    source(file.path(settings[['stat_path']], 'density_and_ROC_plot.R', fsep = .Platform$file.sep))
    
  
  # Init PROCESS Parameters -----------------------------------------------------------------  
  
    # TODO! Empty input as well need to be accepted
  
    # NOTE! The name should be the same as found from the Master Data sheet
  
      # get rid of patients that do not match there, for example useful if 
      # you want to save a reduced data frame to disk
  
      # "PRIMARY VARIABLES", used also for matching if there is any
      parameters[['main_factor']] = 'Diagnosis'
      
      parameters[['factors_keep']][[parameters[['main_factor']]]] = 
        c('Control', 'POAG', 'NTG', 'DISC SUSPECT')
        
      parameters[['factors_keep']][[parameters[['main_factor']]]] = 
        c('Control', 'NTG', 'POAG')
      
      # Secondary, tertiary, factors if needed
      # parameters[['factors_keep']][['Race']] = c(1)
      
    # If you want to match let's say by age, getting rid of the non-matching ages for example
      
      parameters[['match_reference']] = c('POAG', 'NTG', 'DISC SUSPECT')
      parameters[['matched_by']][['Age']] = 12 # threshold in years

    # If you want to use custom column to select the subject to use
      
      parameters[['handpick_subjects']][['Flag']] = TRUE
      parameters[['handpick_subjects']][['Column']] = 'InterimGlaucomavsControl2'
      
    # The features of interest
      parameters[['factors']] = parameters[['main_factor']] # fixed variable
      # parameters[['features']] = c('MaxConstr', 'QuickPhasic', '6SecondPIPR', 'SlopesON_2_phasic', 
      #                              'SlopesOFF_2_phasic', 'DFA_Hest', 'MFDFA_spectrum_width_hq', 'MFDFA_spectrum_peak_hq')
    
      parameters[['features']] = c('MaxConstr', 'QuickPhasic', '6SecondPIPR')
      
      
    # The traces of interest
      
      time_col = "time_onsetZero"
      pupil_col = "pupil" # Change this if you for example want to average the "Base", etc.
      error_col = "error_fractional"
      
      # TODO! Error with the selection!!
      
      parameters[['traces']][['x']] = time_col
      parameters[['traces']][['y']] = pupil_col
      parameters[['traces']][['error']] = error_col
    
      # STATS
      categorical_vars = list()
      categorical_vars[[1]] = c(parameters[['main_factor']])
      categorical_vars[[2]] = c(parameters[['main_factor']], 'Sex')
      parameters[['stats']][['categorical_var_names']] = c('Diagnosis', 'Diagnosis*Sex')
      parameters[['stats']][['categorical_vars']] = categorical_vars
      parameters[['stats']][['prelim_tests']][['p_threshold']] = 0.05
      parameters[['stats']][['pairwise_tests']][['p_threshold']] = 0.05
      
      # ROC
      parameters[['ROC']][['combine_pathologies_also']] = TRUE
      
    # TODO! You could read all the parameter pairs from .txt files, and have one row
    # per comparison and simply then loop through the rows giving you all the desired 
    # stats with a button push
    
      # now the [1] is a placeholder for the 1st row
      analysis_param = list()
      i = 1
      analysis_param[[i]] = parameters
      
      # copy the same values to second entry
      i = i +1
      analysis_param[[i]] = parameters
      
      # and change the values need to be changed
      analysis_param[[i]][['handpick_subjects']][['Flag']] = FALSE
      analysis_param[[i]][['match_reference']] = c('DM')
      analysis_param[[i]][['factors_keep']][[parameters[['main_factor']]]] = 
                                      c('Control', 'DM')
    
  # PROCESS -----------------------------------------------------------------  
    
    no_of_different_analyses = length(analysis_param)
    
    # Now go through all the analyses wanted
    for (analys_ind in 1 : no_of_different_analyses) {
    
      # Plot average traces
      master_indices_out = plot.average.traces(data_frame_feats, list_traces, subject_codes_traces, 
                               dataset_type, derived_feats_names,
                               analysis_param[[analys_ind]], settings)
      
      # Show the feats then
      p2 = stats.point.features(data_frame_feats, list_traces, dataset_type,
                                master_indices_out, derived_feats_names, plot_type = 'boxplot',
                                parameters = analysis_param[[analys_ind]], settings)
              
    }
      
}



