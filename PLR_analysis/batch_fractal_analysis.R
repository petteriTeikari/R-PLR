# Initialize --------------------------------------------------------------

  library(doParallel)  
  
  # Define Paths
  script.dir <- dirname(sys.frame(1)$ofile)
  
  data_path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/reconstructed/simplified'
  data_path_out = file.path(data_path, '..', '..', 'fractalAnalysis', 'MFDFA', fsep = .Platform$file.sep)
  
  # data_path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/reconstructed/'
  # data_path_out = file.path(data_path, '..', 'timeFrequency', fsep = .Platform$file.sep)
  
  source_path = file.path(script.dir, 'subfunctions', fsep = .Platform$file.sep)
  IO_path = file.path(script.dir, '..', 'PLR_IO', fsep = .Platform$file.sep)
  config_path = file.path(script.dir, '..', 'config', fsep = .Platform$file.sep)
  
  source(file.path(source_path, 'file_fractal_wrapper.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'fractal_monofractal_wrapper.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'fractal_multifractal_wrapper.R', fsep = .Platform$file.sep))
  
  # Check input files
  pattern_to_find = "*.csv"
  files_to_process = list.files(path=data_path, pattern=pattern_to_find, recursive=FALSE, full.names = TRUE)
  
  # Parameters
  param = list()
  param[['no_of_cores_to_use']] = detectCores()
  
# COMPUTATIONS

  start_time <- Sys.time()  
  list_of_DFs = mclapply(files_to_process, function(files_to_process){
    file.fractal.wrapper(files_to_process, data_path_out, param, debug = FALSE)
  }, mc.cores = param[['no_of_cores_to_use']])
  end_time <- Sys.time()
  end_time - start_time # 5.367922 mins with home AMD (4 cores, 388 files)
  
  