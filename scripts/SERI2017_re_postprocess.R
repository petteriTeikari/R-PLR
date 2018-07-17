SERI2017.re.postprocess = function() {
  
  # Define computer-specific paths
  # TODO! Make more adaptive
  paths = list()
  paths[['RPLR']][['base']] = '/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR'
  paths[['data_in']][['base']] = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/SERI_2017'
  paths[['data_out']][['base']] = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/SERI_2017_EMD'
  
  # Init scripts
  source(file.path(paths[['RPLR']][['base']], 'clean_and_reconstruct_all_PLR.R', fsep = .Platform$file.sep))
  paths = init.paths.and.functions(paths)
  paths = source.subfunctions(paths)
  import.and.install.libraries()
  param = init.PLR.processing.params(paths)
  paths[['data_in']][['EMD']] = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/SERI_2017_imputed'
  paths[['data_out']][['EMD_fusion']] = file.path(paths[['data_out']][['base']], 'IMF_fusion', fsep = .Platform$file.sep)
  paths[['data_out']][['EMD_denoising']] = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/SERI_2017_denoisingFusion'
  
  library(doParallel)
  registerDoParallel(cores=2)
  
  # Impute the missing values
  batch.AnalyzeAndReImpute(data_path = paths[['data_in']][['base']], 
                           RPLR_recon_path = paths[['recon']],
                           parameters = param[['recon']],
                           RPLR_paths = paths[['RPLR']],
                           masterExcel = paths[['data_in']][['excelMasterPath']],
                           process_only_unprocessed = FALSE, # no need to re-process all 400 files
                           path_check_for_done = paths[['data_out']][['reconstructed']], 
                           pupil_col = 'pupil',
                           combine_with_database = FALSE,
                           vars_to_keep = c('time', 'pupil', 'error'))

  # Run Empirical Mode Decomposition for denoising
  batch.EMD.decomposition(data_path = paths[['data_in']][['EMD']], 
                          data_path_out = paths[['data_out']][['base']],
                          RPLR_recon_path = paths[['recon']],
                          parameters = param[['recon']],
                          RPLR_paths = paths[['RPLR']],
                          masterExcel = paths[['data_in']][['excelMasterPath']],
                          process_only_unprocessed = FALSE,
                          path_check_for_done = paths[['data_out']][['reconstructed']],
                          pupil_col = 'pupil')
  
  # Do quick'n'dirty fusion of the EMD results (with our without proper fusion)
  batch.auto.EMD.fusion(data_path = paths[['data_in']][['EMD']], 
                        data_path_EMD = paths[['data_out']][['base']],
                        data_path_EMD_fusion =paths[['data_out']][['EMD_fusion']],
                        data_path_out = paths[['data_out']][['EMD_denoising']],
                        RPLR_recon_path = paths[['recon']],
                        parameters = param[['recon']],
                        RPLR_paths = paths[['RPLR']],
                        masterExcel = paths[['data_in']][['excelMasterPath']],
                        process_only_unprocessed = FALSE,
                        path_check_for_done = paths[['data_out']][['reconstructed']],
                        pupil_col = 'pupil')
  
  
  
}