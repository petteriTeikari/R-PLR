analyze.and.reimpute = function(filename_path, data_path_out, param, pupil_col = 'pupil', debug = FALSE) {
  
  just_filename = tail(strsplit(filename_path, .Platform$file.sep)[[1]], 1)
  filecode = strsplit(just_filename, '_')[[1]][1]
  cat(filecode, ' ')
  
  df_in = read.csv(filename_path)
  
  # And get the previously corrected labels
  gt_outlier_corr = is.na(df_in$pupil_outlier_corrected)
  sum_1stPass = sum(gt_outlier_corr)
  
  if (length(df_in[[pupil_col]]) == 0) {
    # not found this field
    pupil_col = 'pupil'
    gt_2nd_pass = vector(, length = length(df_in[[pupil_col]]))
  } else {
    gt_2nd_pass = is.na(df_in[[pupil_col]])
    sum_2ndPass = sum(gt_2nd_pass)
    
  }
  no_of_nans = sum(is.na(df_in[[pupil_col]]))
  
  # Use easier variable names
  t = df_in$time_onsetZero
  y = df_in[[pupil_col]]
  error_frac = df_in$error_fractional
  weights = 1 / ((error_frac*y)^2)
  weights_norm = weights / max(weights, na.rm = TRUE)
  
  if (no_of_nans > 0) {
    
    y_na = imp.imputeTS.wrapper(df_in, t, y, error_frac, weights_norm, filecode, 'imputeTS_kalman_StructTS', param)
    no_of_nans2 = sum(is.na(y_na))
    
    # Output data frame
    df_out = df_in
    pupil_col_2 = paste0(pupil_col, '_StructTS_iter')
    df_out[[pupil_col_2]] = y_na$pupil
    # df_out[['pupil']] = y_na$pupil
    
  } else {
    
    df_out = df_in
    na_vector = vector(, length = length(df_in$pupil))
    na_vector[] = NA
    pupil_col_2 = paste0(pupil_col, '_StructTS_iter')
    df_out[[pupil_col_2]] = na_vector
    
  }
  
  
  # Get final outlier labels
  gt_out = gt_outlier_corr | gt_2nd_pass
  sum_out = sum(gt_out)
  
  # Use for MissFOREST or some other machine learning / deep learning training
  pupil_toBeImputed = df_in$pupil_outlier_corrected
  pupil_toBeImputed[gt_out] = NA
  
  gt_out_integer = as.integer(gt_out)
  df_out[['pupil_toBeImputed']] = pupil_toBeImputed
  df_out[['outlier_labels']] = gt_out_integer
 
  # DEBUG
  if (debug) {
    ggplot(df_out, aes(time_onsetZero, pupil)) +
      geom_line()
  }
  
  cat(' NAs=', sum(is.na(df_out[['pupil']])), ' ')
  
  return(df_out)
  
}
