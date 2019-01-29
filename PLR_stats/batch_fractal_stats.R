# Initialize --------------------------------------------------------------

  # Define Paths
  script.dir <- dirname(sys.frame(1)$ofile)
  
  data_path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/fractalAnalysis/MFDFA'
  data_path_out = file.path(data_path, 'stats', fsep = .Platform$file.sep)

  source_path = file.path(script.dir, 'subfunctions', fsep = .Platform$file.sep)
  IO_path = file.path(script.dir, '..', 'PLR_IO', fsep = .Platform$file.sep)
  config_path = file.path(script.dir, '..', 'config', fsep = .Platform$file.sep)
  
  # Check input files
  pattern_to_find = "*.RData"
  files_to_process = list.files(path=data_path, pattern=pattern_to_find, recursive=FALSE, full.names = TRUE)

# COMPUTATIONS

