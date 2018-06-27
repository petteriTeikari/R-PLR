# Initialize --------------------------------------------------------------

  # Define Paths
  script.dir <- dirname(sys.frame(1)$ofile)
  
  #  Make programmatic?
  # source('/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/PLR_reconstruction/batch_PLR_reconstruction.R')
  masterXLS_data_path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder'
  data_path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/imputation_final'
  data_path_out = file.path(data_path, '..', 'recon_EMD', fsep = .Platform$file.sep)
  # data_path_out = data_path
  source_path = file.path(script.dir, 'subfunctions', fsep = .Platform$file.sep)
  IO_path = file.path(script.dir, '..', 'PLR_IO', fsep = .Platform$file.sep)
  config_path = file.path(script.dir, '..', 'config', fsep = .Platform$file.sep)
  
  source(file.path(source_path, 'init_reconstruction.R', fsep = .Platform$file.sep))
  paths_for_output = init.reconstruction(script.dir, data_path, source_path, IO_path) # Init script
    
  # Check input files
  pattern_to_find = "*missForest.csv"
  files_to_process = get.files.for.reconstruction(data_path, pattern_to_find)
  
  # Parameters
  param = list()
  param[['fps']] = 30
  param[['no_of_cores_to_use']] = detectCores() 

# CEEMD Decomposition ----------------------------------------------------------------
  
  # Works on any POSIX-like operating system (Linux, Mac OS X, etc—basically all but Windows)
  # https://www.r-bloggers.com/a-no-bs-guide-to-the-basics-of-parallelization-in-r/
  # https://github.com/tdhock/mclapply-memory
  start_time <- Sys.time()
  list_of_DFs = mclapply(files_to_process, function(files_to_process){
    file.decomposition.wrapper(files_to_process, data_path_out, param, debug = FALSE)
  }, mc.cores = param[['no_of_cores_to_use']])
  end_time <- Sys.time()
  end_time - start_time # 17.26166 hours with home AMD (4 cores)
  