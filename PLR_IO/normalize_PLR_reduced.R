normalize.PLR.reduced = function(df, config_path, 
                                 value_operator = 'median',
                                 normalize_method = 'hybrid') {
  
  # define baseline
  bins = import.binDefinitions(config_path)
  baseline_period = get.baseline.period.from.bins(bins)
  baseline_period = c(-5, 0) # overwrite
  # warning('Manual overwrite of the baseline from bins, TODO! fix this')
  b_i1 = which.min(abs(baseline_period[1] - df$time_onsetZero))
  b_i2 = which.min(abs(baseline_period[2] - df$time_onsetZero))
  
  baseline_vector = df$pupil_outlier_corrected[b_i1:b_i2]
  
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
                   'time_onsetZero', 'time_maxDeriv_zero')
  
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
  df$error = df$error_fractional * df$pupil
  
  return(df)
  
}