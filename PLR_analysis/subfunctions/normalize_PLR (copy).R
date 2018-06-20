normalize_PLR = function(data_frame_in, color, bins, 
                         normalize_on, normalize_method, normalize_indiv_colors, 
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
  
    var_ind = 1
    if_pupil_value = grepl(vars_to_normalize[var_ind], 'pupil')
    
    
    if (identical(normalize_method, 'hybrid')) {
      # $CE$4 is the baseline value (median)
      # =100*(($CE$4-$CI197)/$CE$4)
      
      if (if_pupil_value == TRUE) {
        data_frame_out[[ vars_to_normalize[var_ind] ]] =
        ( baseline - data_frame_out[[ vars_to_normalize[var_ind] ]] ) /
          data_frame_out[[ vars_to_normalize[var_ind] ]]
        
      } else {
        
      }
      
    } else if (identical(normalize_method, 'divisive')) {
      # (corrected pupilsize = pupilsize/baseline)
      
      # no the same operation for both the pupil sizes and the error
      data_frame_out[[ vars_to_normalize[var_ind] ]] = 
        data_frame_out[[ vars_to_normalize[var_ind] ]] / baseline
      
    } else if (identical(normalize_method, 'subtractive')) {
      # subtractive baseline correction
      # corrected pupilsize = pupilsize − baseline)
      # subtractive baseline correction ->  increased statistical power 
      # more than divisive correction
      
      if (if_pupil_value == TRUE) {
        data_frame_out[[ vars_to_normalize[var_ind] ]] = 
          data_frame_out[[ vars_to_normalize[var_ind] ]] - baseline
      else {
       # no effect on the error the subtraction has 
      }
      
    } else {
      warning('Your normalization method "', normalize_method, '" is not defined! Typo?')
    }
  
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
  
  return(data_frame_out)
  
}

