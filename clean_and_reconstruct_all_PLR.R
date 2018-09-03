clean.and.reconstruct.PLR = function() {
  
  # Define computer-specific paths
  # TODO! Make more adaptive
  paths = list()
  
  # TODO! Read this location automagically
  # Now this ghetto fix for small number of users
  if (identical(toString(Sys.info()["nodename"]), "RAY-XPS" ) &&
      identical(toString(Sys.info()["login"]), "Ray P. Najjar")) {
    
      paths[['RPLR']][['base']] = 'C:/Users/Ray-Najjar/Desktop/GitPLR/R-PLR'
           
   } else if (identical(toString(Sys.info()["nodename"]), "petteri-Server")) {
     
      paths[['RPLR']][['base']] = '/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR'
      
   } else {
     
     warning('No path defined for: ', Sys.info())
   }
  
  
  # paths[['data_in']][['base']] = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA'
  # paths[['data_out']][['base']] = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT'
  paths[['data_in']][['base']] = file.path(paths[['RPLR']][['base']], '..', 'TEST_IN', fsep = .Platform$file.sep)
  paths[['data_out']][['base']] = file.path(paths[['RPLR']][['base']], '..', 'TEST_OUT', fsep = .Platform$file.sep) 
  
  # Init scripts
  source(file.path(paths[['RPLR']][['base']], 'clean_and_reconstruct_all_PLR.R', fsep = .Platform$file.sep))
  paths = init.paths.and.functions(paths)
  paths = source.subfunctions(paths)
  import.and.install.libraries(paths)
  param = init.PLR.processing.params(paths)

  # TODO! Aggregate all libaries loaded with automatic installation to one file
  # TODO! Aggregate all the parameters as well to one ininitalization script
  
  # Import the _BR files
  batch.PLR.videos(data_path = paths[['data_in']][['video']], 
                   RPLR_video_path = paths[['video']], 
                   out_path = paths[['data_out']][['base']],
                   parameters = param[['video']],
                   RPLR_paths = paths[['RPLR']],
                   process_only_unprocessed = TRUE, # no need to re-process all 400 files
                   path_check_for_done = paths[['data_out']][['reconstructed']])
  
  # Remove the artifacts
  batch.PLR.artifacts(data_path = paths[['data_in']][['artifacts']], 
                       RPLR_artifacts_path = paths[['artifacts']],
                       parameters = param[['artifacts']],
                       RPLR_paths = paths[['RPLR']],
                       process_only_unprocessed = TRUE, # no need to re-process all 400 files
                       path_check_for_done = paths[['data_out']][['reconstructed']])
      
  warning('Inspect the outlier now manually, or change the path from next block if you do not want to do it')
  warning('You NEED TO MANUALLY RUN ""')
  
  # Resample to same length
  batch.PLR.resample(data_path = paths[['data_in']][['resampling_corr']], 
                      RPLR_recon_path = paths[['recon']],
                      parameters = param[['artifacts']],
                      RPLR_paths = paths[['RPLR']],
                      masterExcel = paths[['data_in']][['excelMasterPath']],
                      process_only_unprocessed = TRUE, # no need to re-process all 400 files
                      path_check_for_done = paths[['data_out']][['reconstructed']],
                      data_path_alternative = paths[['data_in']][['resampling']]) 
  
  # Impute the missing values
  batch.AnalyzeAndReImpute(data_path = paths[['data_in']][['imputation']], 
                            RPLR_recon_path = paths[['recon']],
                            parameters = param[['recon']],
                            RPLR_paths = paths[['RPLR']],
                            masterExcel = paths[['data_in']][['excelMasterPath']],
                            process_only_unprocessed = TRUE, # no need to re-process all 400 files
                            path_check_for_done = paths[['data_out']][['reconstructed']], 
                            pupil_col = 'pupil',
                            miss_forest_parallelize = 'no',
                            combine_with_database = FALSE,
                            database_path = paths[['data_out']][['reconstructed']])
  
  warning('Inspect the imputation now manually, or change the path from next block if you do not want to do it')
  
  # AFTER MANUAL INSPECTION
  # Iterate this, with changed input folder
  # TODO! Check the normalizations at this point as there seem to be some glitches!
  batch.AnalyzeAndReImpute(data_path = paths[['data_in']][['imputation_rerun']], 
                           RPLR_recon_path = paths[['recon']],
                           parameters = param[['recon']],
                           RPLR_paths = paths[['RPLR']],
                           masterExcel = paths[['data_in']][['excelMasterPath']],
                           process_only_unprocessed = TRUE, # no need to re-process all 400 files
                           path_check_for_done = paths[['data_out']][['reconstructed']], 
                           pupil_col = 'pupil',
                           miss_forest_parallelize = 'no',
                           combine_with_database = FALSE,
                           database_path = paths[['data_out']][['reconstructed']],
                           iterate_imputation = TRUE)
  
  # Run Empirical Mode Decomposition for denoising
  batch.EMD.decomposition(data_path = paths[['data_in']][['EMD']], 
                          data_path_out = paths[['data_out']][['EMD']],
                          RPLR_recon_path = paths[['recon']],
                          parameters = param[['recon']],
                          RPLR_paths = paths[['RPLR']],
                          masterExcel = paths[['data_in']][['excelMasterPath']],
                          process_only_unprocessed = TRUE,
                          path_check_for_done = paths[['data_out']][['reconstructed']],
                          pupil_col = 'pupil')
  
  # Now we have the results a bit scattered around and we cant to combine them
  # NOTE! Re-normalizing all the different pupil size columns inside of this
  combine.data.from.multiple.folders(path_main = paths[['data_out']][['base']], 
                                     RPLR_scripts_path = paths[['RPLR']][['scripts']],
                                     subfolder_paths = c('imputation_final',
                                                         file.path('recon_EMD', 'DONE', fsep = .Platform$file.sep),
                                                         file.path('recon_EMD', 'IMF_fusion', fsep = .Platform$file.sep)), 
                                     patterns = c('*.csv',
                                                  '*.csv',
                                                  '*_signals.csv'))
  
  # Do some semi-intelligent decompositions for machine learning data augmentation purposes
  # Computes as well 1st and 2nd derivatives (i.e. velocity and acceleration) from the smoothed PLRs
  batch.data.decompose.for.augmentation(data_path = paths[['data_out']][['reconstructed']], 
                                        data_path_out = paths[['data_out']][['FinalOUT']],
                                        RPLR_recon_path = paths[['recon']],
                                        parameters = param[['decomp_augm']],
                                        RPLR_paths = paths[['RPLR']],
                                        masterExcel = paths[['data_in']][['excelMasterPath']],
                                        process_only_unprocessed = TRUE,
                                        path_check_for_done = paths[['data_out']][['FinalOUT']],
                                        pupil_col = 'denoised')
  
  # Finally compute the hand-crafted features here
  batch.PLR.analyze.reconstructions(data_path =  paths[['data_in']][['features']], 
                                     data_path_out = paths[['data_out']][['features']],
                                     RPLR_analysis_path = paths[['analysis']],
                                     parameters, RPLR_paths, masterExcel,
                                     process_only_unprocessed = TRUE,
                                     path_check_for_done = paths[['data_out']][['features']], 
                                     no_of_cores_to_use = detectCores(),
                                     pupil_col = 'pupil')

}


# SUBFUNCTION



init.paths.and.functions = function(paths) {
  
  # TODO! If you actually change these, the batch_ functions atm have
  # hard-coded variable names
  # Debugging the "SERI syntax"
  
  # Create output
  if (dir.exists(paths[['data_out']][['base']]) == FALSE) {
    dir.create(paths[['data_out']][['base']], showWarnings = TRUE, recursive = FALSE, mode = "0777")
    cat('Creating the directory for DATA OUTPUT\n')
  }
  
  # Derived paths
  paths[['data_out']][['video']] = file.path(paths[['data_out']][['base']], 'VIDEO')
  paths[['data_out']][['artifacts']] = file.path(paths[['data_out']][['base']], 'outlier_free') 
  paths[['data_out']][['artifacts_corrected']] = file.path(paths[['data_out']][['artifacts']], 'outlier_free_corrected') 
  paths[['data_out']][['resampling']] = file.path(paths[['data_out']][['artifacts']], 'recon_resampled')
  paths[['data_out']][['imputed']] = file.path(paths[['data_out']][['base']], 'imputation_final')
  paths[['data_out']][['EMD']] = file.path(paths[['data_out']][['base']], 'recon_EMD') 
  paths[['data_out']][['EMD_fusion']] = file.path(paths[['data_out']][['EMD']], 'IMF_fusion') 
  paths[['data_out']][['EMD_recomposed']] = file.path(paths[['data_out']][['base']], 'recon_EMD_subcomp_fusion') 
  paths[['data_out']][['reconstruction']] = file.path(paths[['data_out']][['base']], 'recon')
  paths[['data_out']][['reconstructed']] = file.path(paths[['data_out']][['base']], 'reconstructed') 
  paths[['data_out']][['FinalOUT']] = file.path(paths[['data_out']][['base']], 'FinalOUT') 
  paths[['data_out']][['features']] = file.path(paths[['data_out']][['base']], 'PLR_feat') 
  
  # Create outlier_free
  if (dir.exists(paths[['data_out']][['artifacts']]) == FALSE) {
    dir.create(paths[['data_out']][['artifacts']], showWarnings = TRUE, recursive = FALSE, mode = "0777")
    cat('Creating the directory for DATA OUTLIER_FREE\n')
  }
  
  # inputs from these outputs
  paths[['data_in']][['video']] = paths[['data_in']][['base']]
  paths[['data_in']][['artifacts']] = paths[['data_out']][['video']]
  paths[['data_in']][['resampling']] = paths[['data_out']][['artifacts']]
  paths[['data_in']][['resampling_corr']] = paths[['data_out']][['artifacts_corrected']]
  paths[['data_in']][['imputation']] = paths[['data_out']][['resampling']]
  paths[['data_in']][['imputation_rerun']] = file.path(paths[['data_out']][['base']], 
                                                       'recon_imputation_correction', fsep = .Platform$file.sep)
  paths[['data_in']][['EMD']] = paths[['data_out']][['imputed']]
  paths[['data_in']][['features']] = paths[['data_out']][['FinalOUT']]
  
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
  paths[['analysis']] =  file.path(paths[['RPLR']][['base']], 'PLR_analysis', fsep = .Platform$file.sep)
  
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
  
  source(file.path(paths[['RPLR']][['IO']], 'compute_PLR_derivatives.R', fsep = .Platform$file.sep))
  
  source(file.path(paths[['recon']], 'batch_data_decompose_for_augmentation.R', fsep = .Platform$file.sep))
  source(file.path(paths[['recon']], 'subfunctions', 'file_decomp_augmentation_wrapper.R', fsep = .Platform$file.sep))
  
  source(file.path(paths[['analysis']], 'batch_PLR_analyze_reconstructions.R', fsep = .Platform$file.sep))
  
  return(paths)
  
}

import.and.install.libraries = function(paths) {
  
  # TODO!
  # Put ALL THE LIBRARIES needed here
  # https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
  
  cat('Checking the LIBRARIES\n')
  
  if (!require("data.table")) install.packages("data.table"); library("data.table")
  
  # VIDEO
  if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
  
  # ARTIFACTS
  # if (!require("forecast")) install.packages("forecast"); library("forecast")
  if (!require("changepoint")) install.packages("changepoint"); library("changepoint")
  
  # RECONSTRUCTION
  subfunction_path = file.path(paths[['recon']], 'subfunctions', fsep = .Platform$file.sep)
  source(file.path(subfunction_path, 'init_reconstruction.R', fsep = .Platform$file.sep))
  paths_for_output = init.reconstruction(script.dir = paths[['recon']], 
                                         data_path = paths[['data_in']][['resampling_corr']], 
                                         source_path = subfunction_path, 
                                         IO_path = paths[['RPLR']][['IO']]) 
  
  
  # FRACTAL ANALYSIS
  if (!require("MFDFA")) install.packages("MFDFA"); library("MFDFA")
  if (!require("fractal")) install.packages("fractal"); library("fractal")
  if (!require("nonlinearTseries")) install.packages("nonlinearTseries"); library("nonlinearTseries")
    # IF configure: error: missing required header GL/gl.h
    # ERROR: configuration failed for package ‘rgl’
    # THEN: sudo apt-get install libglu1-mesa-dev
    # For fresh installation, you might need these
    # sudo apt-get install libglu1-mesa-dev libcurl4 libfreetype6-dev libcurl4-openssl-dev
    # For Apple / Mac, you need XQuartz unstalled
  
  # TIME-FREQ
  
  
  
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
  param[['decomp_augm']][['dummy']] = NA
  
  return(param)
  
}

