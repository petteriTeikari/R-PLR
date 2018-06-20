# Initialize --------------------------------------------------------------

  # Define Paths
  script.dir <- dirname(sys.frame(1)$ofile)
  
  #  Make programmatic?
  # source('/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/PLR_reconstruction/batch_PLR_reconstruction.R')
  masterXLS_data_path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder'
  data_path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/outlier_free_corrected'
  source_path = file.path(script.dir, 'subfunctions', fsep = .Platform$file.sep)
  IO_path = file.path(script.dir, 'PLR_IO', fsep = .Platform$file.sep)
  config_path = file.path(IO_path, 'config', fsep = .Platform$file.sep)
  
  source(file.path(source_path, 'helper_functions.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'plot_functions.R', fsep = .Platform$file.sep))
  
  # Init script
  paths_for_output = init.reconstruction(script.dir, data_path, source_path, IO_path) 
    
  # Parameters 
  fps = 30
  pattern_to_find = "*_outlier_free_corrected.csv"
  time_lims = c(-15.5, 50.5) # manually defined trim points
                             # TODO! Check this if your paradigm changes
  
  param = list()
  param[['paths']][['script.dir']] = script.dir 
  param[['paths']][['source_path']] = source_path
  vars_to_keep = c('time_onsetZero', 'pupil_outlier_corrected', 'error_fractional')
  
  combine_mode = 're_define_time'
  
  # get the file listing
  files_to_process = get.files.for.reconstruction(data_path, pattern_to_find)
 
  # Get time limits of the input
  time_range = lapply(files_to_process, function(filepath){
    get.timing.stats.of.inputs(data_path, paths_for_output[[1]], IO_path, 
                               filepath, param, time_lims)
  })
  
  # Time limits of the all files
  hard_limits = c(460, 1500)
  timLims = dataset.lims(time_range, hard_limits, debugON = FALSE, fps = fps)
  
  # Get the files for reconstruction
  files_to_process_recon = get.files.for.reconstruction(paths_for_output[[1]], pattern_to_find="*.csv")
  
  if (identical(combine_mode, 'trim_to_shortest')) {
    # Trim the input traces for the same length for easier manipulation
    files_to_process_recon = files_to_process_recon[!timLims[[3]]]
  }
  
  if (length(files_to_process_recon_included) == 0) {
    warning('No files found for trimming/time re-definition?')
  }
  
  # TRIM the input traces for same length
  # 379 files take around 176 Mb of memory
  list_of_PLR_dataframes = lapply(files_to_process_recon, function(filepath){
    create.PLR.dataset.matrix(paths_for_output[[1]], paths_for_output[[2]], paths_for_output[[3]],
                               filepath, fps, timLims[[1]], timLims[[2]], timLims[[3]], timLims[[4]], hard_limits,
                               combine_mode, useAdaptive = FALSE, jitter = FALSE)
  })
  
  # Combine the dataframes in the list of subset of variables into a dataframe
  # for further processing, # 379 files take around 22.7 Mb of memory with 3+1 variables
  subset_of_PLR_list = combine.list.of.dataframes(list_of_PLR_dataframes, vars_to_keep, normalize = TRUE)
  
  # Now we can model the input recordings as we wish using multiple files
  # use easier variable names
  t_mat = subset_of_PLR_list$time_onsetZero
  y_mat = subset_of_PLR_list$pupil_norm
  error_frac_mat = subset_of_PLR_list$error_fractional
  filecodes = get.codes.from.filenames(files_to_process_recon)
  
  XLS_filename = 'Master_File_De-Identified_Copy_For_Petteri.xlsx' 
  master_data_raw = read.theMasterExcel(masterXLS_data_path, XLS_filename)
  master_data_trim = trim.master.data.with.subject.codes(master_data_raw, filecodes)
    master_data = master_data_trim[[1]]
    found_from_master = master_data_trim[[2]]
  
  # You may want to do a lot of things now for the data, so let's use a wrapper
  for (subj in 1 : length(files_to_process_recon)) {
    
    result = tryCatch({
      decomp_df = single.file.reconstruction(t_mat[,subj], y_mat[,subj], error_frac_mat[,subj], filecodes[[subj]], 
                                             master_data, found_from_master, param, vars_to_keep, debugTrim = FALSE)
      
      # Save to disk
      export.pupil.dataframe.toDisk(decomp_df, files_to_process_recon[subj], paths_for_output[[4]], 'recon_CEEMD')
      
    }, finally = {
      
      cat('FILE = ', files_to_process_recon[subj], ' FAILED')
      
    })
    
  }
