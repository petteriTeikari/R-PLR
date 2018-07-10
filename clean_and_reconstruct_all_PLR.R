clean.and.reconstruct.PLR = function() {
  
  # Define computer-specific paths
  # TODO! Make more adaptive
  paths = list()
  paths[['RPLR']][['base']] = '/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR'
  paths[['data_in']][['base']] = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA'
  paths[['data_out']][['base']] = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT'
  
  # Init scripts
  source(file.path(paths[['RPLR']][['base']], 'clean_and_reconstruct_all_PLR.R', fsep = .Platform$file.sep))
  paths = init.paths.and.functions(paths)
  paths = source.subfunctions(paths)
  import.and.install.libraries()
  param = init.PLR.processing.params(paths)

  # TODO! Aggregate all libaries loaded with automatic installation to one file
  # TODO! Aggregate all the parameters as well to one ininitalization script
  
  # Import the _BR files
  batch.PLR.videos(data_path = paths[['data_in']][['video']], 
                   RPLR_video_path = paths[['video']],
                   parameters = param[['video']],
                   RPLR_paths = paths[['RPLR']],
                   process_only_unprocessed = TRUE, # no need to re-process all 400 files
                   path_check_for_done = paths[['data_out']][['Final']])
  
  # Remove the artifacts
  batch.PLR.artifacts(data_path = paths[['data_in']][['artifacts']], 
                       RPLR_artifacts_path = paths[['artifacts']],
                       parameters = param[['artifacts']],
                       RPLR_paths = paths[['RPLR']],
                       process_only_unprocessed = TRUE, # no need to re-process all 400 files
                       path_check_for_done = paths[['data_out']][['EMD']]) # paths[['data_out']][['Final']]) 
      
  # Resample to same length
  batch.PLR.resample(data_path = paths[['data_in']][['resampling_corr']], 
                      RPLR_recon_path = paths[['recon']],
                      parameters = param[['artifacts']],
                      RPLR_paths = paths[['RPLR']],
                      masterExcel = paths[['data_in']][['excelMasterPath']],
                      process_only_unprocessed = TRUE, # no need to re-process all 400 files
                      path_check_for_done = paths[['data_out']][['Final']],
                      data_path_alternative = paths[['data_in']][['resampling']]) 
  
  # Impute the missing values
  batch.AnalyzeAndReImpute(data_path = paths[['data_in']][['imputation']], 
                            RPLR_recon_path = paths[['recon']],
                            parameters = param[['recon']],
                            RPLR_paths = paths[['RPLR']],
                            masterExcel = paths[['data_in']][['excelMasterPath']],
                            process_only_unprocessed = TRUE, # no need to re-process all 400 files
                            path_check_for_done = paths[['data_out']][['Final']], 
                            pupil_col = 'pupil',
                            combine_with_database = TRUE,
                            database_path = paths[['data_out']][['Final']])
  
  # AFTER MANUAL INSPECTION
  # Iterate this, with changed input folder
  batch.AnalyzeAndReImpute(data_path = paths[['data_in']][['imputation_rerun']], 
                           RPLR_recon_path = paths[['recon']],
                           parameters = param[['recon']],
                           RPLR_paths = paths[['RPLR']],
                           masterExcel = paths[['data_in']][['excelMasterPath']],
                           process_only_unprocessed = TRUE, # no need to re-process all 400 files
                           path_check_for_done = paths[['data_out']][['Final']], 
                           pupil_col = 'pupil',
                           combine_with_database = TRUE,
                           database_path = paths[['data_out']][['Final']],
                           iterate_imputation = TRUE)
  
  # Run Empirical Mode Decomposition for denoising
  batch.EMD.decomposition(data_path = paths[['data_in']][['EMD']], 
                          RPLR_recon_path = paths[['recon']],
                          parameters = param[['recon']],
                          RPLR_paths = paths[['RPLR']],
                          masterExcel = paths[['data_in']][['excelMasterPath']],
                          process_only_unprocessed = TRUE,
                          path_check_for_done = paths[['data_out']][['Final']],
                          pupil_col = 'pupil')
  
  # Run Empirical Mode Decomposition for denoising
  batch.EMD.decomposition(data_path = paths[['data_out']][['EMD_fusion']], 
                          RPLR_recon_path = paths[['recon']],
                          parameters = param[['recon']],
                          RPLR_paths = paths[['RPLR']],
                          masterExcel = paths[['data_in']][['excelMasterPath']],
                          process_only_unprocessed = FALSE,
                          path_check_for_done = paths[['data_out']][['Final']],
                          pupil_col = 'loFreq')
  
  batch.EMD.decomposition(data_path = paths[['data_out']][['EMD_fusion']], 
                          RPLR_recon_path = paths[['recon']],
                          parameters = param[['recon']],
                          RPLR_paths = paths[['RPLR']],
                          masterExcel = paths[['data_in']][['excelMasterPath']],
                          process_only_unprocessed = FALSE,
                          path_check_for_done = paths[['data_out']][['Final']],
                          pupil_col = 'hiFreq')
  
  

  batch.recompose.EMD.subdecompositions()

  
  
  
  # Now we have the results a bit scattered around and we cant to combine them
  paths[['RPLR']][['scripts']]
  combine.data.from.multiple.folders(path_main = paths[['data_out']][['base']], 
                                      RPLR_scripts_path = paths[['RPLR']][['scripts']],
                                      subfolder_paths = c('imputation_final',
                                                          'recon_EMD',
                                                          'recon_EMD_subcomp_fusion',
                                                          file.path('recon_EMD', 'IMF_fusion', fsep = .Platform$file.sep)), 
                                     patterns = c('*.csv',
                                                  '*.csv',
                                                  '*.csv',
                                                  '*_fusion.csv'))
  
  # Compute 1st and 2nd derivatives (i.e. velocity and acceleration) from 
  # the EMD denoised pupil signals
  
}


# SUBFUNCTION
init.paths.and.functions = function(paths) {
  
  # TODO! If you actually change these, the batch_ functions atm have
  # hard-coded variable names
  
  # Derived paths
  paths[['data_out']][['video']] = file.path(paths[['data_out']][['base']], 'VIDEO')
  paths[['data_out']][['artifacts']] = file.path(paths[['data_out']][['base']], 'outlier_free') 
  paths[['data_out']][['artifacts_corrected']] = file.path(paths[['data_out']][['base']], 'outlier_free_corrected') 
  paths[['data_out']][['resampling']] = file.path(paths[['data_out']][['base']], 'recon_resampled')
  paths[['data_out']][['imputed']] = file.path(paths[['data_out']][['base']], 'imputation_final')
  paths[['data_out']][['EMD']] = file.path(paths[['data_out']][['base']], 'recon_EMD') 
  paths[['data_out']][['EMD_fusion']] = file.path(paths[['data_out']][['EMD']], 'IMF_fusion') 
  paths[['data_out']][['EMD_recomposed']] = file.path(paths[['data_out']][['base']], 'recon_EMD_subcomp_fusion') 
  paths[['data_out']][['reconstruction']] = file.path(paths[['data_out']][['base']], 'recon')
  paths[['data_out']][['Final']] = file.path(paths[['data_out']][['base']], 'reconstructed') 
  
  # inputs from these outputs
  paths[['data_in']][['video']] = paths[['data_in']][['base']]
  paths[['data_in']][['artifacts']] = paths[['data_out']][['video']]
  paths[['data_in']][['resampling']] = paths[['data_out']][['artifacts']]
  paths[['data_in']][['resampling_corr']] = paths[['data_out']][['artifacts_corrected']]
  paths[['data_in']][['imputation']] = paths[['data_out']][['resampling']]
  paths[['data_in']][['imputation_rerun']] = file.path(paths[['data_out']][['base']], 
                                                       'recon_imputation_correction', fsep = .Platform$file.sep)
  paths[['data_in']][['EMD']] = paths[['data_out']][['imputed']]
  
  # Excel Master data sheet
  paths[['data_in']][['masterSheet']] = file.path(paths[['data_out']][['base']], '..', fsep = .Platform$file.sep)
  paths[['data_in']][['masterFile']] = 'Master_File_De-Identified_Copy_For_Petteri.xlsx' 
  paths[['data_in']][['excelMasterPath']] = file.path(paths[['data_in']][['masterSheet']], 
                                                 paths[['data_in']][['masterFile']], fsep = .Platform$file.sep)
  
  # TODO! Actually you want to read from the manually reconstructed ones
  paths[['data_in']][['reconstruction']] = paths[['data_out']][['artifacts']]
  # paths[['data_in']][['reconstruction']] = paths[['data_out']][['artifacts_corrected']]
                                      
  # define the package paths
  paths[['video']] = file.path(paths[['RPLR']][['base']], 'PLR_video', fsep = .Platform$file.sep)
  paths[['artifacts']] = file.path(paths[['RPLR']][['base']], 'PLR_artifacts', fsep = .Platform$file.sep)
  paths[['recon']] =  file.path(paths[['RPLR']][['base']], 'PLR_reconstruction', fsep = .Platform$file.sep)
  
  # R paths
  paths[['RPLR']][['IO']] = file.path(paths[['RPLR']][['base']], 'PLR_IO', fsep = .Platform$file.sep)
  paths[['RPLR']][['scripts']] = file.path(paths[['RPLR']][['base']], 'scripts', fsep = .Platform$file.sep)
  
  # Source the main processing subfunctions
  source(file.path(paths[['video']], 'batch_PLR_videos.R', fsep = .Platform$file.sep))
  source(file.path(paths[['artifacts']], 'batch_PLR_artifacts.R', fsep = .Platform$file.sep))
  source(file.path(paths[['recon']], 'subfunctions', 'init_reconstruction.R', fsep = .Platform$file.sep))
  source(file.path(paths[['recon']], 'batch_PLR_resample.R', fsep = .Platform$file.sep))
  source(file.path(paths[['recon']], 'batch_AnalyzeAndReImpute.R', fsep = .Platform$file.sep))
  source(file.path(paths[['recon']], 'batch_EMD_decomposition.R', fsep = .Platform$file.sep))
  source(file.path(paths[['recon']], 'batch_recompose_EMD_subdecompositions.R', fsep = .Platform$file.sep))
  source(file.path(paths[['RPLR']][['scripts']], 'combine_data_from_multiple_folders.R', fsep = .Platform$file.sep))
  
  
  return(paths)
  
}

import.and.install.libraries = function() {
  
  # install.packages("data.table")
  library(data.table)

  
}

source.subfunctions = function(paths) {
  
  source(file.path(paths[['RPLR']][['IO']], 'check_for_done_filecodes.R', fsep = .Platform$file.sep))
  return(paths)
  
}

init.PLR.processing.params = function(paths) {
  
  param = list()
  param[['video']][['dummy']] = NA
  param[['artifacts']][['dummy']] = NA
  param[['recon']][['dummy']] = NA
  
  return(param)
  
}
