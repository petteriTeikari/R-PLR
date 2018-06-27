# Initialize --------------------------------------------------------------

  # Define Paths
  script.dir <- dirname(sys.frame(1)$ofile)
  
  #  Make programmatic?
  # source('/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/PLR_reconstruction/batch_PLR_reconstruction.R')
  masterXLS_data_path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder'
  data_path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/recon_imputation_correction'
  data_path_out = file.path(data_path, '..', 'imputation_final', fsep = .Platform$file.sep)
  source_path = file.path(script.dir, 'subfunctions', fsep = .Platform$file.sep)
  IO_path = file.path(script.dir, '..', 'PLR_IO', fsep = .Platform$file.sep)
  config_path = file.path(script.dir, '..', 'config', fsep = .Platform$file.sep)
  
  source(file.path(source_path, 'init_reconstruction.R', fsep = .Platform$file.sep))
  paths_for_output = init.reconstruction(script.dir, data_path, source_path, IO_path) # Init script
    
  # Check input files
  pattern_to_find = "*_imputation_corrected.csv"
  files_to_process = get.files.for.reconstruction(data_path, pattern_to_find)
  
  # Parameters
  param = list()
  param[['fps']] = 30

# REIMPUTE ----------------------------------------------------------------
  
  # First with the TSImpute, file-by-file
  list_of_DFs = lapply(files_to_process, function(files_to_process){
    analyze.and.reimpute(files_to_process, data_path_out, param, pupil_col = 'pupil_outlier_corrected')
  })
  
  vars_to_keep = c('time_onsetZero', 'pupil_toBeImputed', 'outlier_labels', 'error_fractional')
  vars_as_matrices = combine.list.of.dataframes(list_of_DFs, vars_to_keep)
  # now we have for example 4 matrices that you could save as .csv files if you want these outside R

  # Heavier lifting with MissForest
  imputed_missForest = impute.with.MissForest(vars_as_matrices, pupil_col = 'pupil_toBeImputed')
    matrix_imputed = imputed_missForest[[1]]
    OOBerror = imputed_missForest[[2]]

  # TODO! Add maybe included variables here, or later to get harmonize the number of columns
  
  # combine the result with the list of DFs
  for (i in 1 : length(list_of_DFs)) {
    
    df_out = list_of_DFs[[i]]
    df_out[['missForest']] = matrix_imputed[,i]
    df_out[['pupil']] = matrix_imputed[,i]
    
    export.pupil.dataframe.toDisk(df_out, 
                                  files_to_process[i], 
                                  data_path_out, 'missForest')
  }
    