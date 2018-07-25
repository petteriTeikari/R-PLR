normalize.PLR.reduced = function(df, config_path = NA, 
                                 value_operator = 'median',
                                 normalize_method = 'hybrid',
                                 denormalize_first = FALSE,
                                 global_baseline = TRUE,
                                 pupil_col = 'pupil',
                                 indices = NA) {
  
  # define baseline
  if (is.na(indices[1])) {
    bins = import.binDefinitions(config_path)
    baseline_period = get.baseline.period.from.bins(bins)
    b_i1 = which.min(abs(baseline_period[1] - df$time_onsetZero))
    b_i2 = which.min(abs(baseline_period[2] - df$time_onsetZero))
  } else {
    b_i1 = indices[1]
    b_i2 = indices[2]
  }
  
  baseline_vector = df[[pupil_col]][b_i1:b_i2]
    
  # the stats of the vector
  baseline_stats = data.frame(mu = mean(baseline_vector, na.rm = TRUE),
                                med = median(baseline_vector, na.rm = TRUE),
                                stdev = sd(baseline_vector, na.rm = TRUE),
                                variance = var(baseline_vector, na.rm = TRUE),
                                n = length(baseline_vector))
    
  if (identical(value_operator, 'median')) {
    baseline = baseline_stats$med
  } else if (identical(value_operator, 'mean')) {
    baseline = baseline_stats$mu
  } else {
    warning(value_operator, ' not accepted as "value_operator" using "median" now!')
    baseline = baseline_stats$med
  }
  
  vars_names_in = colnames(df)
  
  # these variables are not meant to be normalized
  # TODO!
  # REMEMBER: that if you add new variables to data_frame, this part
  # is not exactly adaptive
  vars_to_excl = c('frame', 'time', 'x', 'y', 'X.', 'X', 'X.1', 'X.2', 'X.3',
                   'R_center', 'G_center', 'B_center', 
                   'R', 'G', 'B', 
                   'error_fractional', 'video_fractional_error', 
                   'spline_error_fractional',
                   'safe_vector', 
                   'exluded_points', 'included_points', 
                   'exluded_points_2ndPass', 'included_points_2ndPass',
                   'handplaced_points',
                   'time_onsetZero', 'time_maxDeriv_zero',
                   'imputeTS_kalman_StructTS_error_frac',
                   'outlier_labels', 'CEEMD_IMF_1',
                   'CEEMD_IMF_2', 'CEEMD_IMF_3', 'CEEMD_IMF_4',
                   'CEEMD_IMF_5', 'CEEMD_IMF_6', 'CEEMD_IMF_7',
                   'CEEMD_IMF_8', 'CEEMD_IMF_9', 'CEEMD_IMF_10',
                   'CEEMD_IMF_11', 'residue', 'hinstfreq_1',
                   'hinstfreq_2', 'hinstfreq_3', 'hinstfreq_4',
                   'hinstfreq_5', 'hinstfreq_6', 'hinstfreq_7',
                   'hinstfreq_8', 'hinstfreq_9', 'hinstfreq_10',
                   'hinstfreq_11', 'hamp_1', 'hamp_2', 'hamp_3',
                   'hamp_4', 'hamp_5', 'hamp_6', 'hamp_7', 'hamp_8', 
                   'hamp_9', 'hamp_10', 'hamp_11', 'base_osc', 
                   'oscillations', 'noiseNorm', 'noiseNonNorm',
                   'hiFreq', 'loFreq', 'base', 'smooth',
                   'denoised',
                   'light_on', 'light_state_str', 'light_state',
                    'baseline', 'baseline_on')
  
  to_keep_indices = is.na(match(vars_names_in, vars_to_excl))
  vars_to_normalize = vars_names_in[to_keep_indices]
  
  for (var_ind in 1:length(vars_to_normalize)) {
    
    # if pupil size in this variable instead of an error measure
    # define just by the name
    var_name =  vars_to_normalize[var_ind]
    if_pupil_value = grepl('pupil', var_name)
    
    # cat(var_ind, ': ',var_name, ' - ', if_pupil_value, '\n')
    
    df[[var_name]] = normalize.low.level(df[[var_name]], 
                                         baseline, normalize_method, if_pupil_value)
    
  }
  
  # Write the baseline to the data frame as well
  baseline_vector_out = vector(length=length(df$time))
  baseline_vector_out[] = baseline # replicated the scalar as a vector
  df$baseline = baseline_vector_out
  
  # median
  baseline_vector_out[] = baseline_stats$med
  df$baseline_median = baseline_vector_out
  
  # mean
  baseline_vector_out[] = baseline_stats$mu
  df$baseline_mean = baseline_vector_out
  
  # finally correct the error based on just the error_fractional?
  if (length(df$error_fractional) != 0) {
    df$error = df$error_fractional * df$pupil  
  }
  
  
  return(df)
  
}