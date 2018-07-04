clean.and.reconstruct.PLR = function(path_base = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder') {
  
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
                       path_check_for_done = paths[['data_out']][['Final']]) 
      
  # Reconstruct the traces (i.e. impute and denoise)
  
}

# SUBFUNCTION
init.paths.and.functions = function(paths) {
  
  # TODO! If you actually change these, the batch_ functions atm have
  # hard-coded variable names
  
  # Derived paths
  paths[['data_out']][['video']] = file.path(paths[['data_out']][['base']], 'VIDEO')
  paths[['data_out']][['artifacts']] = file.path(paths[['data_out']][['base']], 'outlier_free') 
  paths[['data_out']][['artifacts_corrected']] = file.path(paths[['data_out']][['base']], 'outlier_free_corrected') 
  paths[['data_out']][['imputed']] = file.path(paths[['data_out']][['base']], 'imputation_final')
  paths[['data_out']][['reconstruction']] = file.path(paths[['data_out']][['base']], 'recon')
  paths[['data_out']][['EMD']] = file.path(paths[['data_out']][['base']], 'recon_EMD') 
  paths[['data_out']][['EMD_fusion']] = file.path(paths[['data_out']][['EMD']], 'IMF_fusion') 
  paths[['data_out']][['Final']] = file.path(paths[['data_out']][['base']], 'reconstructed') 
  
  # inputs from these outputs
  paths[['data_in']][['video']] = paths[['data_in']][['base']]
  paths[['data_in']][['artifacts']] = paths[['data_out']][['video']]
  paths[['data_in']][['reconstruction']] = paths[['data_out']][['artifacts']]
  paths[['data_in']][['reconstruction_corr']] = paths[['data_out']][['artifacts_corrected']]
  
  # TODO! Actually you want to read from the manually reconstructed ones
  paths[['data_in']][['reconstruction']] = paths[['data_out']][['artifacts']]
  # paths[['data_in']][['reconstruction']] = paths[['data_out']][['artifacts_corrected']]
                                      
  # define the package paths
  paths[['video']] = file.path(paths[['RPLR']][['base']], 'PLR_video', fsep = .Platform$file.sep)
  paths[['artifacts']] = file.path(paths[['RPLR']][['base']], 'PLR_artifacts', fsep = .Platform$file.sep)
  paths[['recon']] =  file.path(paths[['RPLR']][['base']], 'PLR_reconstruction', fsep = .Platform$file.sep)
  
  # R paths
  paths[['RPLR']][['IO']] = file.path(paths[['RPLR']][['base']], 'PLR_IO', fsep = .Platform$file.sep)
  
  # Source the main processing subfunctions
  source(file.path(paths[['video']], 'batch_PLR_videos.R', fsep = .Platform$file.sep))
  source(file.path(paths[['artifacts']], 'batch_PLR_artifacts.R', fsep = .Platform$file.sep))
  # source(file.path(paths[['recon']], 'batch_PLR_reconstruction.R', fsep = .Platform$file.sep))
  
  return(paths)
  
}

source.subfunctions = function(paths) {
  
  source(file.path(paths[['RPLR']][['IO']], 'check_for_done_filecodes.R', fsep = .Platform$file.sep))
  
  return(paths)
  
}

init.PLR.processing.params = function(paths) {
  
  param = list()
  param[['video']][['dummy']] = NA
  param[['artifacts']][['dummy']] = NA
  
  return(param)
  
}

