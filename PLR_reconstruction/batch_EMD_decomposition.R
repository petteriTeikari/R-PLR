batch.EMD.decomposition = function(data_path = NA, data_path_out = NA,
                                    RPLR_recon_path = NA,
                                    parameters, RPLR_paths, masterExcel,
                                    process_only_unprocessed = FALSE,
                                    path_check_for_done, 
                                    pupil_col = 'pupil',
                                    dataset_string = 'none') {

  # Initialize --------------------------------------------------------------

  # Define Paths
  if (is.na(RPLR_recon_path)) {
    script.dir <- dirname(sys.frame(1)$ofile)
    # data_path = file.path(script.dir, 'data', fsep = .Platform$file.sep)
    paths_for_output = init.reconstruction(script.dir, data_path, source_path, IO_path) 
  } else {
    script.dir = RPLR_recon_path
  }

  if (is.na(data_path_out)) {
    data_path_out = file.path(data_path, '..', paste0('recon_EMD_', pupil_col), fsep = .Platform$file.sep)
    data_path_out = file.path(data_path, '..', 'recon_EMD', fsep = .Platform$file.sep)
  }
  source_path = file.path(script.dir, 'subfunctions', fsep = .Platform$file.sep)
  IO_path = file.path(script.dir, '..', 'PLR_IO', fsep = .Platform$file.sep)
  config_path = file.path(script.dir, '..', 'config', fsep = .Platform$file.sep)
  
  cat('\nDATA IN: ', data_path, '\n')
  cat('DATA OUT: ', data_path_out, '\n\n')

  source(file.path(source_path, 'init_reconstruction.R', fsep = .Platform$file.sep))
  paths_for_output = init.reconstruction(script.dir, data_path, source_path, IO_path) # Init script
    
  if (identical(dataset_string, 'SERI_median')) {
    time_col = 'time'
  } else {
    time_col = 'time_onsetZero'
  }
  
  # Check input files
  pattern_to_find = "*.csv"
  files_to_process = get.files.for.reconstruction(data_path, pattern_to_find)
  
  if (process_only_unprocessed) {
    indices_undone = check.for.done.filecodes(files_to_process, path_check_for_done)
    files_to_process = files_to_process[indices_undone]
  }
  
  # Parameters
  param = list()
  param[['fps']] = 30
  param[['no_of_cores_to_use']] = detectCores() - 1 # using all but one core 

# CEEMD Decomposition ----------------------------------------------------------------
  
  # Works on any POSIX-like operating system (Linux, Mac OS X, etc—basically all but Windows)
  # https://www.r-bloggers.com/a-no-bs-guide-to-the-basics-of-parallelization-in-r/
  # https://github.com/tdhock/mclapply-memory
  start_time <- Sys.time()
  
  if (identical(.Platform$OS.type, 'windows')) {
    
    warning('You are now running a crappy Windows system that does not support multicore processing :(
    -- thus for example compared to the Ubuntu server, this CEEMD decomposition might be more than 40x slower now')
    
    options(warn = -1)
    list_of_DFs = lapply(files_to_process, function(files_to_process){
      file.decomposition.wrapper(files_to_process, data_path_out, param, pupil_col, debug = FALSE, 
                                 dataset_string, time_col)})
    options(warn = 0)
    
  } else {
  
    options(warn = -1)
    list_of_DFs = mclapply(files_to_process, function(files_to_process){
      file.decomposition.wrapper(files_to_process, data_path_out, param, pupil_col, debug = FALSE, 
                                 dataset_string, time_col)
    } , mc.cores = param[['no_of_cores_to_use']])
    options(warn = 0)
    
  }
  end_time <- Sys.time()
  end_time - start_time # 17.26166 hours with home AMD (4 cores)
  
}
  
