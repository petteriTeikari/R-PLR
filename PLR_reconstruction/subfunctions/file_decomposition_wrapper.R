file.decomposition.wrapper = function(filename_path, data_path_out, param, 
                                      pupil_col, debug = FALSE, 
                                      dataset_string = 'none', time_col = 'time_onsetZero') {

    # Check input
    just_filename = tail(strsplit(filename_path, .Platform$file.sep)[[1]], 1)
    filecode = strsplit(just_filename, '_')[[1]][1]
    cat(filecode, ' ')
  
    # READ IN
    df_in = read.csv(filename_path)
    
    # Easier variable names
    pupilField = pupil_col
    timeField = time_col
    t = df_in[[timeField]]
    y = df_in[[pupilField]]
    error = df_in$error * df_in$error_fractional
    
    if (length(t) == 0) {
      # no time vector found, use linear vector
      t = 1:length(y)
    }
    
    if (length(error) == 0) {
      # no time vector found, use linear vector
      error = vector(, length = length(y))
      error = as.numeric(t) + 1
    }
    
    # Debug
    if (debug) {
      i1 = 300
      i2 = 700
      t = t[i1:i2]
      y = y[i1:i2]
      error = error[i1:i2]
    }
    
    # Parameters
    method_name = 'CEEMD'
    param = list()
    param[['EMD_trials']] = 100
    param[['fps']] = 30
  
    if (sum(is.na(y)) > 0) {
      warning('You have NaNs in your y -vector for decomposition!')
      # Quick'n'dirty interpolation to get stuff through, and you need to fix these later
      warning('Will do a quick imputation, but fix this later for this file')
      y_na = imp.imputeTS.wrapper(df_in, t, y, error, weights_norm = NA, filecode, 'imputeTS_kalman_StructTS', param)
      y = y_na$pupil
      # TODO! Proper Try/Catch
    }
    
    # Actual computation
    decomposed = decomp.EMD.per.subject(t, y, 
                                        filecode, method_name, 
                                        param, debug = FALSE, dataset_string)
    
    # Post process for output
    no_of_imfs_saved = dim(decomposed$imf)[2]
    no_of_samples_per_signal = dim(decomposed$imf)[1] 

    # intrinsic mode functions (IMFs),
    decomp_imf_out = list()
    
    # instantaneous frequencies
    decomp_hinstfreq_out = list() # InstantaneousFrequency(aimf, tt, method = spectral.method, lag = diff.lag)
    
    # instantaneous amplitudes
    decomp_hamp_out = list() # HilbertEnvelope(aimf)
    
    # Go through all IMFs
    for (i in 1 : no_of_imfs_saved) {
      
      # now we save each of the variables to their own list
      var_name = paste(method_name, 'IMF', i, sep='_')
      decomp_imf_out[[var_name]] = decomposed$imf[,i]
      
      var_name = paste('hinstfreq', i, sep='_')
      decomp_hinstfreq_out[[var_name]] = decomposed$hinstfreq[,i]
      
      var_name = paste('hamp', i, sep='_')
      decomp_hamp_out[[var_name]] = decomposed$hamp[,i]
    }
    
    # And still have the residue to save, i.e. the trend
    # the residual left between the signal and IMFs.
    # Not a residual between the signal and the denoised version
    # if you are looking to use EMD for denoising
    var = 'residue'
    residue_signal = decomposed$residue
    
      # Sometimes you don't have any residue left, and residue_signal is empty,
      # so we just save NAs out then
      if (length(residue_signal) == 0) {
        residue_signal = vector(, length = no_of_samples_per_signal)
        residue_signal[] = NA
      }
      decomp_imf_out[[var]] = residue_signal
    
    # Convert the lists to dataframes
    decomp_imf = data.frame(decomp_imf_out)
    decomp_hinstfreq = data.frame(decomp_hinstfreq_out)
    decomp_hamp = data.frame(decomp_hamp_out)
      
    # Combine to a single one
    decomp_df = data.frame(time = t)
    decomp_df = cbind(decomp_df, decomp_imf, decomp_hinstfreq, decomp_hamp)
    
    # Write to disk finally
    export.pupil.dataframe.toDisk(decomp_df, just_filename, data_path_out, 'CEEMD')
    # cat(filecode, ' .. wrote to disk the results')
    
}
