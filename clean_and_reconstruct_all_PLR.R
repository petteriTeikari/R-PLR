clean.and.reconstruct.PLR = function() {

# THE BORING INITIALIZATION PART ------------------------------------------

  # https://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
  library(rstudioapi)    
  full_path = rstudioapi::getActiveDocumentContext()$path
  # script.dir <- dirname(sys.frame(1)$ofile) # https://stackoverflow.com/a/15373917
  
   if (!exists("full_path")) {
    warning(' The location of this file "clean_and_reconstruct_all_PLR.R" for some reason was not detected?\n')
    
    # If this route fails, uncomment next line, and set this manually and then try to keep working line-by-line (Ctrl+Enter)
    # script.dir = 'C:/Users/Ray-Najjar/Desktop/GitPLR/R-PLR' # Windows
    # script.dir = '/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR' # Linux/Mac
    # script.dir = 'C:\\Users\\petteri-sda1\\Dropbox\\manuscriptDrafts\\pupilArtifactsConditioning\\PLR_CODE\\R-PLR'    
    
  } else {
    paths = list()
    script.dir = strsplit(full_path, split = .Platform$file.sep, fixed=TRUE)[[1]]
    if (identical(.Platform$OS.type, 'windows')) {
      # script.dir = strsplit(full_path_script, split = '\\', fixed=TRUE)[[1]]
      # just to make sure that this is correctly split
    }
    just_the_file = tail(script.dir,1)
    cat('   --- just_the_file = ', just_the_file, '\n')
    cat('   --- --- full_path_script = ', full_path, '\n\n')
    script.dir = gsub(just_the_file, '', full_path)
    
    # remove the last separator
    if (substr(script.dir, nchar(script.dir), nchar(script.dir)) == '/') {
      script.dir = substr(script.dir, 1, nchar(script.dir)-1)
    } else if (substr(script.dir, nchar(script.dir), nchar(script.dir)) == '/') {
      script.dir = substr(script.dir, 1, nchar(script.dir)-1)
    }
    paths[['RPLR']][['base']] = script.dir  
  }
  
  # Init scripts
  source(file.path(paths[['RPLR']][['base']], 'clean_and_reconstruct_all_PLR.R', fsep = .Platform$file.sep))
  paths = init.paths.and.functions(paths)
  paths = source.subfunctions(paths)
  import.and.install.libraries(paths)
  param = init.PLR.processing.params(paths)
  

# THE ACTUAL PROCESSING ---------------------------------------------------

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
      
  warning('Petteri: Inspect the outlier now manually, or change the path from next block if you do not want to do it\n,
          You NEED TO MANUALLY RUN the SHINY APP (server.R from "Apps_Shiny/inspect_outliers/") \n,
          Eyeball the details from the GITHUB WIKI if this does not make sense to you')
  
  # Resample to same length
  batch.PLR.resample(data_path = paths[['data_in']][['resampling_corr']], 
                      RPLR_recon_path = paths[['recon']],
                      parameters = param[['artifacts']],
                      RPLR_paths = paths[['RPLR']],
                      masterExcel = paths[['data_in']][['excelMasterPath']],
                      process_only_unprocessed = FALSE, # no need to re-process all 400 files
                      path_check_for_done = paths[['data_out']][['reconstructed']],
                      data_path_alternative = paths[['data_in']][['resampling']]) 
  
  # Impute the missing values
  batch.AnalyzeAndReImpute(data_path = paths[['data_in']][['imputation']], 
                            RPLR_recon_path = paths[['recon']],
                            parameters = param[['recon']],
                            RPLR_paths = paths[['RPLR']],
                            masterExcel = paths[['data_in']][['excelMasterPath']],
                            process_only_unprocessed = FALSE, # no need to re-process all 400 files
                            path_check_for_done = paths[['data_out']][['reconstructed']], 
                            pupil_col = 'pupil',
                            miss_forest_parallelize = 'no',
                            combine_with_database = FALSE,
                            database_path = paths[['data_out']][['reconstructed']])
  
  warning('Petteri: Inspect the imputation now manually, \n
          or change the path from next block if you do not want to do it \n
          Eyeball the details from the GITHUB WIKI if this does not make sense to you')
  
  # AFTER MANUAL INSPECTION
  # Iterate this, with changed input folder
  # TODO! Check the normalizations at this point as there seem to be some glitches!
  batch.AnalyzeAndReImpute(data_path = paths[['data_in']][['imputation_rerun']], 
                           RPLR_recon_path = paths[['recon']],
                           parameters = param[['recon']],
                           RPLR_paths = paths[['RPLR']],
                           masterExcel = paths[['data_in']][['excelMasterPath']],
                           process_only_unprocessed = FALSE, # no need to re-process all 400 files
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
  
  warning('Inspect the EMDs now manually, or change the path from next block if you do not want to do it \n
           Eyeball the details from the GITHUB WIKI if this does not make sense to you')
  
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



# "PRIVATE" SUBFUNCTIONS --------------------------------------------------
init.paths.and.functions = function(paths) {
  
  # TODO! If you actually change these, the batch_ functions atm have
  # hard-coded variable names
  # Debugging the "SERI syntax"
  
  # Relative defaults
  paths[['data_in']][['base']] = file.path(paths[['RPLR']][['base']], '..', 'DATA_IN', fsep = .Platform$file.sep)
  paths[['data_out']][['base']] = file.path(paths[['RPLR']][['base']], '..', 'DATA_OUT', fsep = .Platform$file.sep)
  
  # Read in the default in and out folders from a config text file:
  config_paths = file.path(paths[['RPLR']][['base']], 'config', fsep = .Platform$file.sep)
  config_paths_file = 'paths.csv'
  config_file_path = file.path(config_paths, config_paths_file, fsep = .Platform$file.sep)
  
  cat(paste0('Reading default DATA IN/OUT paths from "', config_paths_file, '", located in: \n\t\t',
      config_paths))
  
  # write this R-PLR base to the 
  path_string = data.frame(base_path = paths[['RPLR']][['base']])
  if (file.exists(config_paths)) {
    cat('Writing the R-PLR base path to disk to be used again by Shiny Apps for example')
    write.csv(path_string, file = file.path(config_paths, 'base_path_temp.txt', fsep = .Platform$file.sep), row.names = FALSE)
  } else {
    warning(' ... Could not find the config directory: ', config_paths)
  }
  
  
  if (file.exists(config_file_path)) {
    paths_cfg = read.csv(config_file_path, header = FALSE, stringsAsFactors = FALSE)
    win_indices = paths_cfg$V2 == 'windows'
  } else {
    warning('Problem in finding the config file for the DATA paths, was trying to open it from:\n\t\t',
            config_paths, '\n\t\t', 
            'file name = ', config_paths_file)
  }
  
  if (identical(.Platform$OS.type, 'windows')) {
    paths_win = paths_cfg$V3[win_indices]
    paths[['data_in']][['base']] = paths_win[1]
    paths[['data_out']][['base']] = paths_win[2]
  } else {
    paths_unix = paths_cfg$V3[!win_indices]
    paths[['data_in']][['base']] = paths_unix[1]
    paths[['data_out']][['base']] = paths_unix[2]
  }
  
  if (!dir.exists(paths[['data_in']][['base']])) {
    warning("PETTERI: Your DATA_IN directory does not exist, you have to define this first!!!
             do this in the subfunction = init.paths.and.functions() located in clean_and_reconstruct_all_PLR.R\n
             see the .csv file in \n\t\t", config_file_path, "\n\t\t and change the default directory\n
             The current data_in directory is set to:\n\t\t", paths[['data_in']][['base']])
  }
  
  # Create output
  if (dir.exists(paths[['data_out']][['base']]) == FALSE) {
    dir.create(paths[['data_out']][['base']], showWarnings = TRUE, recursive = FALSE, mode = "0777")
    cat('\nCreating the directory for DATA OUTPUT\n')
  }
  
  # Derived paths
  paths[['data_out']][['video']] = file.path(paths[['data_out']][['base']], 'VIDEO')
  paths[['data_out']][['artifacts']] = file.path(paths[['data_out']][['base']], 'outlier_free') 
  paths[['data_out']][['artifacts_corrected']] = file.path(paths[['data_out']][['artifacts']], 'outlier_free_corrected') 
  paths[['data_out']][['resampling']] = file.path(paths[['data_out']][['base']], 'recon_resampled')
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
  paths[['data_in']][['imputation_rerun']] = file.path(paths[['data_out']][['imputed']], 
                                                       'recon_imputation_correction', fsep = .Platform$file.sep)
  paths[['data_in']][['EMD']] = paths[['data_out']][['imputed']]
  paths[['data_in']][['features']] = paths[['data_out']][['FinalOUT']]
  
  # Excel Master data sheet
  paths[['data_in']][['masterSheet']] = file.path(paths[['data_out']][['base']], '..', fsep = .Platform$file.sep)
  paths[['data_in']][['masterFile']] = 'Master_File_De-Identified_Copy_For_Petteri.xlsx' 
  paths[['data_in']][['excelMasterPath']] = file.path(paths[['data_in']][['masterSheet']], 
                                                 paths[['data_in']][['masterFile']], fsep = .Platform$file.sep)
  if (!file.exists(paths[['data_in']][['excelMasterPath']])) {
    warning('Could not find the Master Excel Data sheet from:\n',
            paths[['data_in']][['excelMasterPath']])
  }
  
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
  
  cat('Updating all the libraries, if you have outdated packages\n')
  update.packages(ask = FALSE, dependencies = c('Suggests'))
  
  
  cat('Checking that you have the needed LIBRARIES\n')
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
                                         data_path = paths[['data_out']][['artifacts']], # PT: Not totally sure why the data_path is like this
                                         source_path = subfunction_path, 
                                         IO_path = paths[['RPLR']][['IO']],
                                         from_where_call = 'init_the_analysis_script') 
  
  if (!require("segmented")) install.packages("segmented"); library("segmented")
  if (!require("metafor")) install.packages("metafor"); library("metafor")
  
  
  # FRACTAL ANALYSIS
  if (!require("MFDFA")) install.packages("MFDFA"); library("MFDFA")
  if (!require("fractal")) install.packages("fractal"); library("fractal")
  if (!require("nonlinearTseries")) install.packages("nonlinearTseries"); library("nonlinearTseries")
  
    # IF configure: error: missing required header GL/gl.h
    # ERROR: configuration failed for package 
    # THEN: sudo apt-get install libglu1-mesa-dev
    # For fresh installation, you might need these
    # sudo apt-get install libglu1-mesa-dev libcurl4 libfreetype6-dev libcurl4-openssl-dev
    
    # For Apple / Mac, you need XQuartz unstalled
  
  # TIME-FREQ
  
  
  # SHINY APPs
  if (!require("Cairo")) install.packages("Cairo"); library("Cairo")
  # https://gykovacsblog.wordpress.com/2017/05/15/installing-cairo-for-r-on-ubuntu-17-04/
  # sudo apt-get install libcairo2-dev libgtk2.0-dev xvfb xauth xfonts-base libxt-dev
  
  if (!require("rprojroot")) install.packages("rprojroot"); library("rprojroot")
  # https://stackoverflow.com/questions/51956807/current-path-of-an-r-script
  
  if (!require("rstudioapi")) install.packages("rstudioapi"); library("rstudioapi")
  if (!require("moments")) install.packages("moments"); library("moments") # EMD
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

