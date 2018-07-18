file.decomp_augmentation.wrapper = function(filename_path, data_path_out, param, 
                                      pupil_col, 
                                      hi_freq_col = 'hiFreq',
                                      lo_freq_col = 'loFreq', 
                                      noise_cols = c('noiseNorm', 'noiseNonNorm'),
                                      derivative_col = 'smooth_wo_hi',
                                      debug = FALSE) {


  # INIT --------------------------------------------------------------------
  
    # Check input
    just_filename = tail(strsplit(filename_path, .Platform$file.sep)[[1]], 1)
    filecode = strsplit(just_filename, '_')[[1]][1]
    cat(filecode, ' ')
  
    # READ IN
    df_in = read.csv(filename_path)
    
    # Easier variable names
    pupilField = pupil_col
    timeField = 'time_onsetZero'
    t = df_in[[timeField]]
    y = df_in[[pupilField]]
    error = df_in$error * df_in$error_fractional
    
    loFreq = df_in[[lo_freq_col]]
    hiFreq = df_in[[hi_freq_col]]
    
    noise_ind = colnames(df_in) %in% noise_cols
    kept_cols = colnames(df_in)[noise_ind]
    
    if (length(t) == 0) {
      # no time vector found, use linear vector
      t = 1:length(y)
    }
    
    if (length(error) == 0) {
      # no time vector found, use linear vector
      error = vector(, length = length(y))
      error = as.numeric(t) + 1
    }

  # ACTUAL DECOMPOSITION PART -----------------------------------------------

    # quick'n dirty smoothing that a lot of research groups might be doing
    
    loess_model_deg2 = loess(hiFreq~t, span = 0.05, degree = 2)
    residual_hi_deg2 = hiFreq - loess_model_deg2$fitted
    y_smooth_hi = y - residual_hi_deg2

    loess_model_lo = loess(loFreq~t, span = 0.1, degree = 2)
    residual_lo = loFreq - loess_model_lo$fitted
    y_smooth_lo = y_smooth_hi - residual_lo
    
    loess_smooth = loess(y_smooth_lo~t, span = 0.1, degree = 2)
    residual_smooth = y_smooth_lo - loess_smooth$fitted
    y_smoothest = y_smooth_lo - residual_smooth
 
    # plot(t,y,type='l')
    # plot(t,y_smooth_hi,type='l')
    # plot(t,y_smooth_lo,type='l')
    # plot(t,y_smoothest,type='l')
    
    # Just to make sure that the pupil actually is the denoised one
    df_in[['pupil']] = df_in[['denoised']]
    
    # add the smoothed ones
    df_in[['smooth_wo_hi']] = y_smooth_hi
    df_in[['smooth_wo_hi_and_lo']] = y_smooth_lo
    df_in[['smoothest_of_all']] = y_smoothest

  # VELOCITY AND ACCELERATION -----------------------------------------------

    derivatives = compute.PLR.derivatives(t, y = df_in[[derivative_col]])
    
    df_in[['velocity']] = derivatives[[1]]
    df_in[['acceleration']] = derivatives[[2]]
    
    plot(t, df_in[['velocity']], type='l')
    plot(t, df_in[['acceleration']], type='l')
    
    
  # EXPORT ------------------------------------------------------------------
    
    # Write to disk finally
    filename_out = paste0(filecode, '.csv')
    export.pupil.dataframe.toDisk(df_in, filename_out, data_path_out, 'finalData')
    # cat(filecode, ' .. wrote to disk the results')
      
  }
