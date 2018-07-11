batch.AnalyzeAndReImpute = function(data_path = NA, RPLR_recon_path = NA,
                              parameters, RPLR_paths, masterExcel,
                              process_only_unprocessed = FALSE,
                              path_check_for_done, 
                              pupil_col = 'pupil',
                              combine_with_database = TRUE,
                              database_path,
                              iterate_imputation = FALSE) {

  # Initialize --------------------------------------------------------------
  
    # Define Paths
    if (is.na(RPLR_recon_path)) {
      script.dir <- dirname(sys.frame(1)$ofile)
      # data_path = file.path(script.dir, 'data', fsep = .Platform$file.sep)
      source(file.path(source_path, 'init_reconstruction.R', fsep = .Platform$file.sep))
      paths_for_output = init.reconstruction(script.dir, data_path, source_path, IO_path) # Init script
    } else {
      script.dir = RPLR_recon_path
    }
    
    data_path_out = file.path(data_path, '..', 'imputation_final', fsep = .Platform$file.sep)
    source_path = file.path(script.dir, 'subfunctions', fsep = .Platform$file.sep)
    IO_path = file.path(script.dir, '..', 'PLR_IO', fsep = .Platform$file.sep)
    config_path = file.path(script.dir, '..', 'config', fsep = .Platform$file.sep)
    
    paths_for_output = init.reconstruction(script.dir, data_path, source_path, IO_path) 
    
    # Check input files
    pattern_to_find = "*.csv"
    files_to_process = get.files.for.reconstruction(data_path, pattern_to_find)
    
    if (length(files_to_process) == 0) {
      
      if (iterate_imputation) {
        warning('You wanted to iterate the missForest for corrected values, but no corrected\n',
                'files were found from = "', data_path, '"')
      }
      
    # If input files were found
    } else {
      
      if (process_only_unprocessed) {
        indices_undone = check.for.done.filecodes(files_to_process, path_check_for_done)
        files_to_process = files_to_process[indices_undone]
      }
      
      # Parameters
      param = list()
      param[['fps']] = 30
      
      # REIMPUTE ----------------------------------------------------------------
      
      # First with the TSImpute, file-by-file
      list_of_DFs = lapply(files_to_process, function(files_to_process){
        analyze.and.reimpute(files_to_process, data_path_out, param, pupil_col = pupil_col)
      })
      no_of_new_files = length(list_of_DFs)
      
      vars_to_keep = c('time_onsetZero', 'pupil_toBeImputed', 'outlier_labels', 'error_fractional')
      vars_as_matrices = combine.list.of.dataframes(list_of_DFs, vars_to_keep)
      # now we have for example 4 matrices that you could save as .csv files if you want these outside R
      
      if (combine_with_database) {
        vars_as_matrices = combine.new.files.with.database(database_path, data_path_out, param, 
                                                           vars_as_matrices, pupil_col, vars_to_keep)
      }
      
      # Heavier lifting with MissForest
      imputed_missForest = impute.with.MissForest(vars_as_matrices, pupil_col = 'pupil_toBeImputed')
      matrix_imputed = imputed_missForest[[1]]
      OOBerror = imputed_missForest[[2]]
      
      if (combine_with_database) {
        matrix_imputed = matrix_imputed[,1:no_of_new_files]
      }  
      
      # TODO! Add maybe included variables here, or later to get harmonize the number of columns
      
      # combine the result with the list of DFs
      for (i in 1 : length(list_of_DFs)) {
        
        df_out = list_of_DFs[[i]]
        df_out[['missForest']] = matrix_imputed[,i]
        df_out[['pupil']] = matrix_imputed[,i]
        
        just_filename = tail(strsplit(files_to_process[i], .Platform$file.sep)[[1]], 1)
        filecode = strsplit(just_filename, '_')[[1]][1]
        
        export.pupil.dataframe.toDisk(df_out, 
                                      paste0(filecode, '.csv'),
                                      data_path_out, 'missForest')
      }
    }
      
}
 



combine.new.files.with.database = function(database_path, data_path_out, param, 
                                           vars_as_matrices, pupil_col, vars_to_keep) {
  
  pattern_to_find = "*.csv"
  db_files_to_process = get.files.for.reconstruction(database_path, pattern_to_find)
  
  cat('\n')
  cat('Importing the database values for MissForest imputation\n')
  db_list_of_DFs = lapply(db_files_to_process, function(db_files_to_process){
    analyze.and.reimpute(db_files_to_process, data_path_out, param, 
                         pupil_col = pupil_col, verbose = FALSE)
  })
  db_vars_as_matrices = combine.list.of.dataframes(db_list_of_DFs, vars_to_keep)
  # sum(is.na(db_vars_as_matrices))
  
  # COMBINE
  colnames = names(db_vars_as_matrices)
  vars_as_matrices2 = list() 
  for (i in 1 : length(colnames)) {
    vars_as_matrices2[[colnames[i]]] = cbind(vars_as_matrices[[colnames[i]]], 
                                             db_vars_as_matrices[[colnames[i]]])
  }
  
  cat('\n')
  return(vars_as_matrices2)
  
}