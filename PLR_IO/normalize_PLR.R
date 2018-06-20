normalize_PLR = function(data_frame_in, color ='blue', bins, 
                         normalize_on = TRUE, normalize_method = 'hybrid', normalize_indiv_colors = FALSE, 
                         baseline_period, light_range) {
  
  # Assign the input to output
  data_frame_out = data_frame_in
  
  # Use median instead of mean
  value_operator = 'median'
  
  # Use a subfunction
  data_frame_out = normalize.all.variables(data_frame_out, baseline_stats, value_operator, 
                                           color, light_range, baseline_period,
                                           normalize_method)
  
}
  
# Subfunction to go through all the relevant variables in the data frame
normalize.all.variables = function(data_frame_out, baseline_stats, value_operator, 
                                   color, light_range, baseline_period,
                                   normalize_method) {
  
  # define the PLR data points for the baseline
  data_name = 'pupil'
  baseline_vector = define.baseline.points(data_frame_out, data_name, color, light_range, baseline_period)
  
  # the stats of the vector
  baseline_stats = data.frame(mu = mean(baseline_vector),
                              med = median(baseline_vector),
                              stdev = sd(baseline_vector),
                              variance = var(baseline_vector),
                              n = length(baseline_vector))
  
  vars_names_in = colnames(data_frame_out)
  
  # these variables are not meant to be normalized
  # REMEMBER: that if you add new variables to data_frame, this part
  # is not exactly adative
  vars_to_excl = c('frame', 'time', 'x', 'y', 'X.', 
                   'R_center', 'G_center', 'B_center', 
                   'R', 'G', 'B', 
                   'error_fractional', 'video_fractional_error', 
                   'spline_error_fractional',
                   'safe_vector')
  
  to_keep = is.na(match(vars_names_in, vars_to_excl))
  vars_to_normalize = vars_names_in[to_keep]
  
  if (identical(value_operator, 'median')) {
    baseline = baseline_stats$med
  } else if (identical(value_operator, 'mean')) {
    baseline = baseline_stats$mu
  } else {
    warning(value_operator, ' not accepted as "value_operator" using "median" now!')
    baseline = baseline_stats$med
  }
  
  for (var_ind in 1:length(vars_to_normalize)) {
    
    # if pupil size in this variable instead of an error measure
    # define just by the name
    if_pupil_value = grepl(vars_to_normalize[var_ind], 'pupil')
    var_name =  vars_to_normalize[var_ind]
    
    # DEBUG
    # str(var_name)
    # str(if_pupil_value)
    # str(baseline)
    # str(normalize_method)
    
    data_frame_out[[var_name]] = normalize.low.level(data_frame_out[[var_name]], 
                                                     baseline, 
                                                     normalize_method, 
                                                     if_pupil_value)
    
  }
  
  # Write the baseline to the data frame as well
  baseline_vector_out = vector(length=length(data_frame_out$time))
  baseline_vector_out[] = baseline # replicated the scalar as a vector
  data_frame_out$baseline = baseline_vector_out
  
  # median
  baseline_vector_out[] = baseline_stats$med
  data_frame_out$baseline_median = baseline_vector_out
  
  # mean
  baseline_vector_out[] = baseline_stats$mu
  data_frame_out$baseline_mean = baseline_vector_out
  
  # finally correct the error based on just the error_fractional?
  data_frame_out$error = data_frame_out$error_fractional * data_frame_out$pupil
  
  return(data_frame_out)
  
}

