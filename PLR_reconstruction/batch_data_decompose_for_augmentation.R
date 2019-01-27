batch.data.decompose.for.augmentation = function(data_path = NA, 
                                                 data_path_out = NA,
                                                 RPLR_recon_path = NA,
                                                 parameters, RPLR_paths, masterExcel,
                                                 process_only_unprocessed = FALSE,
                                                 path_check_for_done, 
                                                 pupil_col = 'pupil') {
  
  # Initialize --------------------------------------------------------------
  
    # TODO! initialize part is more or less the same for all the batch_* files
    # now so you could make this a common init_script at some point
  
    # Define Paths
    if (is.na(RPLR_recon_path)) {
      script.dir <- dirname(sys.frame(1)$ofile)
      # data_path = file.path(script.dir, 'data', fsep = .Platform$file.sep)
      paths_for_output = init.reconstruction(script.dir, data_path, source_path, IO_path) 
    } else {
      script.dir = RPLR_recon_path
    }
    
    # data_path_out = file.path(data_path, '..', 'reconstructed_withAugm_andAll', fsep = .Platform$file.sep)
    source_path = file.path(script.dir, 'subfunctions', fsep = .Platform$file.sep)
    IO_path = file.path(script.dir, '..', 'PLR_IO', fsep = .Platform$file.sep)
    config_path = file.path(script.dir, '..', 'config', fsep = .Platform$file.sep)
    
    source(file.path(source_path, 'init_reconstruction.R', fsep = .Platform$file.sep))
    paths_for_output = init.reconstruction(script.dir, data_path, source_path, IO_path) # Init script
    
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
    param[['no_of_cores_to_use']] = detectCores() 
    
  # Go through the files
    
    start_time <- Sys.time()
    if (identical(.Platform$OS.type, 'windows')) {
      list_of_DFs = lapply(files_to_process, function(files_to_process){
        file.decomp_augmentation.wrapper(files_to_process, data_path_out, param, pupil_col, debug = FALSE)
      }) 
      
    } else {
      list_of_DFs = mclapply(files_to_process, function(files_to_process){
        file.decomp_augmentation.wrapper(files_to_process, data_path_out, param, pupil_col, debug = FALSE)
      } , mc.cores = param[['no_of_cores_to_use']])
    }
    end_time <- Sys.time()
    end_time - start_time # 3 seconds for 3 files
  
  
}