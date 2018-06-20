get.datapoints.per.bin = function(data_frame_in, timingMethod, startTime, endTime, 
                                  startString, light_range, color) {
  
  if (identical(color, 'blue')) {
    inds_L = light_range$blue
  } else if (identical(color, 'red')) {
    inds_L = light_range$red
  } else {
    warning("Typo with color = '", color, "' or you just have not yet defined this?\n")  
  }
  
  # Check if there are Inf -value
  # This will happen if no particular light was on during the recording
  if (is.infinite(inds_L[1]) | is.infinite(inds_L[2])) {
    # warning('Cannot extract datapoints for the color = ', color)
  }
  
  # Correct the start and end times if they have "-1" which indicate the start or end
  # of the 
  if (endTime == -1) {
    if (identical(startString, 'onset')) {
      endTime_ind = inds_L[2] # end of light onset
      endTime = data_frame_in$time[endTime_ind]
      
      # remove the start time to keep this end time consistent with the bins.csv definitions
      endTime = endTime - data_frame_in$time[inds_L[1]]
      
    } else if (identical(startString, 'offset')) {
      if (identical(color, 'blue')) {
        endTime_ind = light_range$red[1] - 1 # just before red onset
        endTime = data_frame_in$time[endTime_ind]
        endTime = endTime - data_frame_in$time[inds_L[2]]
      } else if (identical(color, 'red')) {
        endTime_ind = length(data_frame_in$time) # until the end of the recording
        endTime = data_frame_in$time[endTime_ind]
        endTime = endTime - data_frame_in$time[inds_L[2]]
      }
    }
  
    
  } else if (startTime == -1) {
    warning('Not yet implemented this! The start index as -1')
    
  } else if (startTime < 0 | endTime < 0) {
    warning('Negative bin timing limits not allowed beyond the use of "-1"')
  }
  
  # The "bins.csv" defines time in seconds, whereas we are selecting ranges
  # using indices so convert them to indices.
  # In case we do not have a match between the desired time point and time vector,
  # we find the minimum between the difference of desired point and vector
  diff_start = abs(data_frame_in$time - startTime)
  diff_end = abs(data_frame_in$time - endTime)
  start_ind = which.min(diff_start) 
  end_ind = which.min(diff_end)
  
  # No this becomes NA if the color was not ON during recording
  # If for example only BLUE light was ON during recording
  no_range_defined = FALSE
  if (is.na(endTime) | is.infinite(inds_L[2])) {
    warning('No range defined for this bin')
    no_range_defined = TRUE
  }
  
  range_in_ind = end_ind - start_ind
  if (length(range_in_ind > 0)) {
    if (range_in_ind < 0) {
      warning("Something fishy here as your start index is bigger than your end index")
    }
  } else {
    # warning('No range indices for data bin defined')
  }
  
  if (no_range_defined == FALSE) {
  
    # initialize inds
    inds = vector('integer', length=2)
    
    # If bin is defined from the start of the recording
    if(identical(timingMethod, 'recording')) {
      inds[1] = start_ind # start
      inds[2] = end_ind # end
      
      # If bin is defined from Light ONSET
      # add the "bins.csv" -defined values to the _detected_ light ONSET
    } else if(identical(timingMethod, 'onset')) {
      inds[1] = start_ind + inds_L[1] # 
      inds[2] = end_ind + inds_L[1] # end
      
      # add the "bins.csv" -defined values to the _detected_ light OFFSET
    } else if(identical(timingMethod, 'offset')) {
      inds[1] = start_ind + inds_L[2] # start
      inds[2] = end_ind + inds_L[2] # end
      
    } else {
      warning("Typo with timingMethod = '", color, "' or you just have not yet defined this? Check your bins.csv\n")  
    }
    
    data_points = trim.all.the.df.variables(data_frame_in, inds, no_range_defined) 
    
  } else {
    
    # warning('Indices for bin not valid, nothing extracted')
    data_points = trim.all.the.df.variables(data_frame_in, inds, no_range_defined) 
    
  }
  
  # Keep the original time vector before correction
  data_points$time_orig = data_points$time
  
  # Correct for the light onset the time vector (so that it is at 0)
  light_onset_in_seconds = data_frame_in$time[inds_L[1]]
  light_offset_in_seconds = data_frame_in$time[inds_L[2]]
  if (identical(timingMethod, 'onset') | identical(timingMethod, 'recording')) {
    data_points$time = data_points$time - light_onset_in_seconds
  } else if (identical(timingMethod, 'offset')) {
    data_points$time = data_points$time - light_offset_in_seconds
  }
  
  return(data_points)
  
}

trim.all.the.df.variables = function(data_frame_to_trim, inds, no_range_defined) {
  
  variable_names = colnames(data_frame_to_trim)
  no_of_variables_in = length(variable_names)
  
  # init empty data_frame
  list_trimmed = list()
  
  for (var_index in 1 : no_of_variables_in) {
    
    if (no_range_defined == FALSE) {
      list_trimmed[[ variable_names [var_index] ]] =
        data_frame_to_trim[[ variable_names [var_index] ]][inds[1]:inds[2]]
    } else {
      list_trimmed[[ variable_names [var_index] ]] = NA
    }
    
  }
  
  data_frame_trimmed = data.frame(list_trimmed)
  
  return(data_frame_trimmed)
  
  
}
