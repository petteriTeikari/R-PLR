batch.recompose.EMD.subdecompositions = function(data_path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT',
                                                  data_subpaths = c('recon_EMD',
                                                                 'recon_EMD_hiFreq',
                                                                 'recon_EMD_loFreq'),
                                                  noise_names = c('noise', 'noiseNorm', 'noiseNonNorm'),
                                                  oscillation_names = c('loFreq_hi', 'hiFreq_lo', 'hiFreq_hi'), 
                                                  base_names = c('base', 'loFreq_lo'),
                                                  data_path_out = file.path(data_path, 'recon_EMD_subcomp_fusion')) {
  
  # FILE LISTING ------------------------------------------------------------
  
  no_of_folders = length(data_subpaths)
  
  # Check number of files from first folder
  # TODO! No check if the lengths are the same
  main_folder = file.path(data_path, data_subpaths[1])
  main_files = list.files(path=main_folder, pattern='*.csv', recursive=FALSE, full.names = TRUE)
  no_of_files = length(main_files)
  
  noise_inds = list()
  osc_inds = list()
  base_inds = list()
  
  for (f in 1 : no_of_files) {
  
    for (i in 1 : no_of_folders) {
      
      main_folder = file.path(data_path, data_subpaths[i])
      # main_files = list.files(path=main_folder, pattern='*.csv', recursive=FALSE, full.names = TRUE)
      fusion_folder = file.path(main_folder, 'IMF_fusion')
      fusion_files = list.files(path=fusion_folder, pattern='*.csv', recursive=FALSE, full.names = TRUE)
      
      filecode = strsplit(tail(strsplit(fusion_files[f], .Platform$file.sep)[[1]],1), '_')[[1]][1]
      df_fusion = read.csv(fusion_files[f])
      
      # df_main = read.csv(main_files[f])
      
      # NOISE
      noise_cols = colnames(df_fusion) %in% noise_names
      noise_inds[[i]] = which(noise_cols)
      noise_data = df_fusion[noise_cols]
      noise_data = rowSums(noise_data) # sum all vectors, e.g. noiseNorm and noiseNonNorm
      
      # OSCILLATORY ("DETAIL")
      osc_cols = colnames(df_fusion) %in% oscillation_names
      osc_inds[[i]] = which(osc_cols)
      osc_data = df_fusion[osc_cols]
      osc_data = rowSums(osc_data) # sum all vectors, e.g. noiseNorm and noiseNonNorm
      
      # BASE
      base_cols = colnames(df_fusion) %in% base_names
      base_inds[[i]] = which(base_cols)
      base_data = df_fusion[base_cols]
      base_data = rowSums(base_data) # sum all vectors, e.g. noiseNorm and noiseNonNorm

      if (i == 1) {
        noise_per_subject = noise_data
        osc_per_subject = osc_data
        base_per_subject = base_data
      } else {
        noise_per_subject = noise_per_subject + noise_data
        osc_per_subject = osc_per_subject + osc_data
        base_per_subject = base_per_subject + base_data
        
      }
    }  
    
    timepoints = 1:length(osc_per_subject)
    
    # Add some invariance for data augmentation using other decomposition than EMD
    
    # denoise the noise
    # loess_model = loess(noise_per_subject~timepoints, span = 0.01)
    # noise_smooth = loess_model$fitted
    # 
    # # denoise the residual
    # residuals = loess_model$residuals
    # loess_model = loess(residuals~timepoints, span = 0.010)
    # residuals_smooth = loess_model$fitted
    # 
    # noise_smooth = noise_smooth + residuals_smooth
    # residual_noise = noise_per_subject - noise_smooth
    # 
    # plot(noise_smooth, type='l')
    # plot(residual_noise, type='l')
    # plot(noise_per_subject, type='l')
    # 
    # denoise the oscillations
    loess_model = loess(osc_per_subject~timepoints, span = 0.05)
    oscillation_smooth = loess_model$fitted
    
    # denoise the residual
    loess_model = loess(loess_model$residuals~timepoints, span = 0.05)
    osc_residual_smooth = loess_model$fitted
    
    # denoise the residual of the residuals
    loess_model = loess(osc_residual_smooth~timepoints, span = 0.05)
    osc_residual_of_residual_smooth = loess_model$fitted
    #plot(osc_residual_of_residual_smooth, type='l')
    
    # Sum the smooth parts to be the smooth oscillatory component
    oscillations = oscillation_smooth + osc_residual_smooth + osc_residual_of_residual_smooth
    
    # And the remaining part is then high frequency ripple on top of it
    residuals = osc_per_subject - oscillations
    #plot(oscillations, type='l')
    #plot(residuals, type='l')
    
    df_out = data.frame(time = timepoints,
                        base_osc = base_per_subject,
                        oscillations = osc_per_subject,
                        oscillations_smooth = oscillations,
                        oscillations_hiFreq = residuals,
                        noise = noise_per_subject)
    
    cat(f, ' ')
    export.pupil.dataframe.toDisk(df_out, paste0(filecode, 'signals.csv'), data_path_out, 'recomposed')
    
    # Save also the indices
    data_path_out_indices = file.path(data_path_out, 'Indices', .Platform$file.sep)
    save(base_inds, osc_inds, noise_inds, 
         file = file.path(data_path_out_indices, paste0(filecode, '.RData')))
    
  }
}



