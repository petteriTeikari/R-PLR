batch.PLR.resample = function(data_path = NA, RPLR_recon_path = NA,
                                    parameters, RPLR_paths, masterExcel,
                                    process_only_unprocessed = FALSE,
                                    path_check_for_done,
                                    data_path_alternative) {
  
  # Initialize --------------------------------------------------------------
  
  # Define Paths
  if (is.na(RPLR_recon_path)) {
    script.dir <- dirname(sys.frame(1)$ofile)
    # data_path = file.path(script.dir, 'data', fsep = .Platform$file.sep)
  } else {
    script.dir = RPLR_recon_path
    
  }
  
  #  Make programmatic?
  # source('/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/PLR_reconstruction/batch_PLR_reconstruction.R')
  source_path = file.path(script.dir, 'subfunctions', fsep = .Platform$file.sep)
  IO_path = file.path(script.dir, '..', 'PLR_IO', fsep = .Platform$file.sep)
  config_path = file.path(script.dir, '..', 'config', fsep = .Platform$file.sep)
  
  # Init script
  paths_for_output = init.reconstruction(script.dir, data_path, source_path, IO_path) 
  
  # Parameters 
  fps = 30
  pattern_to_find = "*.csv"
  time_lims = c(-15.5, 50.5) # manually defined trim points
  # TODO! Check this if your paradigm changes
  
  param = list()
  param[['paths']][['script.dir']] = script.dir 
  param[['paths']][['source_path']] = source_path
  vars_to_keep = c('time_onsetZero', 'pupil_outlier_corrected', 'error_fractional')
  
  combine_mode = 're_define_time'
  
  # get the file listing
  files_to_process = get.files.for.reconstruction(data_path, pattern_to_find)
  
  # TODO! data_path_alternative
  if (process_only_unprocessed) {
    indices_undone = check.for.done.filecodes(files_to_process, path_check_for_done)
    files_to_process = files_to_process[indices_undone]
  }
  
  # Get time limits of the input
  time_range = lapply(files_to_process, function(filepath){
    get.timing.stats.of.inputs(data_path, paths_for_output[[1]], IO_path, 
                               filepath, param, time_lims)
  })
  
  # Time limits of the all files
  hard_limits = c(460, 1500)
  timLims = dataset.lims(files_to_process, time_range, hard_limits, time_lims,
                         debugON = FALSE, fps = fps)
  
  # Get the files for reconstruction
  files_to_process_recon = get.files.for.reconstruction(data_path, pattern_to_find="*.csv")
  
  if (identical(combine_mode, 'trim_to_shortest')) {
    # Trim the input traces for the same length for easier manipulation
    files_to_process_recon = files_to_process_recon[!timLims[[3]]]
  }
  
  if (process_only_unprocessed) {
    indices_undone = check.for.done.filecodes(files_to_process_recon, path_check_for_done)
    files_to_process_recon = files_to_process_recon[indices_undone]
  }
  
  if (length(files_to_process_recon) == 0) {
    warning('No files found for trimming/time re-definition?')
  }
  
  # TRIM the input traces for same length
  # 379 files take around 176 Mb of memory
  list_of_PLR_dataframes = lapply(files_to_process_recon, function(filepath){
    create.PLR.dataset.matrix(paths_for_output[[1]], paths_for_output[[2]], paths_for_output[[3]], config_path,
                              filepath, fps, timLims[[1]], timLims[[2]], timLims[[3]], timLims[[4]], hard_limits,
                              combine_mode, useAdaptive = FALSE, jitter = FALSE, 
                              normalize = TRUE, normalize_method = 'hybrid', value_operator = 'median')
  })
  
  # Combine the dataframes in the list of subset of variables into a dataframe
  # for further processing, # 379 files take around 22.7 Mb of memory with 3+1 variables
  # Makes it easier maybe to have 3rd parties along to look at the data without millions
  # of variables along
  subset_of_PLR_list = combine.list.of.dataframes(list_of_PLR_dataframes, vars_to_keep)
  
  # Now we can model the input recordings as we wish using multiple files
  # use easier variable names
  t_mat = subset_of_PLR_list$time_onsetZero
  y_mat = subset_of_PLR_list$pupil
  error_frac_mat = subset_of_PLR_list$error_fractional
  filecodes = get.codes.from.filenames(files_to_process_recon)
  
  master_data_raw = read.theMasterExcel(fullpath = masterExcel)
  
  master_data_trim = trim.master.data.with.subject.codes(master_data_raw, filecodes)
  master_data = master_data_trim[[1]]
  found_from_master = master_data_trim[[2]]
  
  # Impute first data for the missing values, quite fast operation with Kalman
  for (subj in 1 : length(files_to_process_recon)) {
    
    impute_df = single.file.reconstruction(t_mat[,subj], y_mat[,subj], error_frac_mat[,subj], filecodes[[subj]], 
                                           master_data, found_from_master, param, vars_to_keep, debugTrim = FALSE,
                                           operator = 'imputation')
    
    # Combine with the list_of_PLR_dataframes and write to disk
    resampled_and_imputed_df = combine.input.with.reconstruction(list_of_PLR_dataframes[[subj]], impute_df,
                                                                 pupil_string = names(impute_df)[1])
    
    # Save to disk
    export.pupil.dataframe.toDisk(resampled_and_imputed_df, files_to_process_recon[subj], paths_for_output[[2]], 'imputation')
    
  }
  
}
