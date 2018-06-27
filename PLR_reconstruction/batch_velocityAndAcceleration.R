# Initialize --------------------------------------------------------------

  # Define Paths
  script.dir <- dirname(sys.frame(1)$ofile)
  
  data_path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/reconstructed/simplified'
  data_path_out = file.path(data_path, '..', 'velocityAndAcceleration', fsep = .Platform$file.sep)
  
  source_path = file.path(script.dir, 'subfunctions', fsep = .Platform$file.sep)
  IO_path = file.path(script.dir, '..', 'PLR_IO', fsep = .Platform$file.sep)
    
  # Check input files
  pattern_to_find = "*.csv"
  files_to_process = list.files(path=data_path, pattern=pattern_to_find, recursive=FALSE, full.names = TRUE)
  
  source(file.path(source_path, 'file_velocityAndAcceleration_wrapper.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'plot_functions.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'compute_PLR_derivatives.R', fsep = .Platform$file.sep))
  source(file.path(IO_path, 'compute_power_spectrum.R', fsep = .Platform$file.sep))
  
  # Parameters
  param = list()
  param[['fps']] = 30
  param[['no_of_cores_to_use']] = detectCores() 

# CEEMD Decomposition ----------------------------------------------------------------

  start_time <- Sys.time()
  list_of_DFs = mclapply(files_to_process, function(files_to_process){
    file.velocityAndAcceleration.wrapper(files_to_process, data_path_out, param, debug = FALSE)
  }, mc.cores = param[['no_of_cores_to_use']])
  end_time <- Sys.time()
  end_time - start_time # 17.26166 hours with home AMD (4 cores)
  