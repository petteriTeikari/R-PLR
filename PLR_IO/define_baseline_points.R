define.baseline.points = function(data_frame, data_name, color, light_range, baseline_period) { 
  
  # Check that the color input is correct
  if (identical(color, 'blue')) {
    light_onset_index = light_range[[color]][1]  
  } else if (identical(color, 'red')) {
    light_onset_index = light_range[[color]][1]
  } else {
    warning('Based on what color are you actually trying normalize PLR? You gave ', color, '\n')
  }

  # Define times in seconds for the light onset and baseline period
  time_at_light_onset = data_frame$time[light_onset_index]
  baseline_end_time = time_at_light_onset - baseline_period[2]
  baseline_start_time = time_at_light_onset + baseline_period[1]
  
  # Find the corresponding indices for the baseline end-start
  # use again the difference between the baseline period times
  # and the time vector, in case we don't find an exact match
  diff_time_end = abs(data_frame$time - baseline_end_time)
  diff_time_start = abs(data_frame$time - baseline_start_time)
  start_ind = which.min(diff_time_start) 
  end_ind = which.min(diff_time_end)
  
  baseline_points = data_frame[[data_name]][start_ind:end_ind]
  
}