create.PLR.dataset.matrix = function(data_path_out = NA, data_resampled_path_out = NA, data_trimmed_path_out = NA, 
                                     config_path, filename_path, 
                                     fps, time_lims, time_lims_in, exclusion_boolean, time_new, hard_limits,
                                     combine_mode = 're_define_time', useAdaptive = FALSE, jitter = FALSE, 
                                     normalize = TRUE, 
                                     value_operator = 'median', normalize_method = 'hybrid') {
  
  data_frame_in = read.csv(filename_path)
  
  filename_sep = strsplit(filename_path, .Platform$file.sep)[[1]]
  just_filename = tail(filename_sep, n=1)
  just_path = gsub(just_filename, '', filename_path)
  cat('  Reading and trimming file = ', just_filename, ' \n')

  # Get light onset  
  light_range_index = define.whenLightWasOn(data_frame_in)
  light_onset_blue = light_range_index$blue[1]
  
  # And align to the light onset all the traces 
  # TODO! check indexing, now the ones at the end to make sure that the length is not too long
  ind1 = light_onset_blue - hard_limits[1] + 1
  ind2 = light_onset_blue + hard_limits[2]

  # Use the same time vector for all the recordings, and if there are gaps in recording, 
  # they will be marked with NAs
  if (identical(combine_mode, 're_define_time')) {
    
    onset_zeroed_time = data_frame_in$time - data_frame_in$time[light_onset_blue]
    
    length_in = length(time_new)
    new_indices_from_in = get.corresponding.time.indices(onset_zeroed_time, time_new, fps)
    length_out = length(new_indices_from_in)
    if (length_in != length_out) {
      warning('For some reason, now your index length changed to = ',  length_out, ' from = ', length_in)
    }
    
    in_indices_from_new = get.corresponding.time.indices(time_new, onset_zeroed_time, fps)
    
    # remove possible ZEROS from indices
    new_indices_from_in[new_indices_from_in == 0] = NA
    in_indices_from_new[in_indices_from_new == 0] = NA
    
    # remove possible NAs from indices
    new_indices = new_indices_from_in[!is.na(new_indices_from_in)]
    in_indices = in_indices_from_new[!is.na(in_indices_from_new)]
    
    # use the indices for new sampling of the signals
    var_names = colnames(data_frame_in)
    no_of_vars = length(var_names)
    
    vars_to_excl = c('time', 'X', 'X.1', 'time_onsetZero')
    to_process = is.na(match(var_names, vars_to_excl))
    vars_to_process = var_names[to_process]
    
    no_of_samples_out = length(time_new)
    
    data_frame_out = data.frame(time = time_new)
    
    for (var in 1 : length(vars_to_process) ) {
      
      var_name = vars_to_process[var]
      zero_vec = vector(mode='numeric', length = no_of_samples_out)
      zero_vec[] = NA
      data_frame_out[[var_name]] = zero_vec # init with NAs
      
      replacement_values = data_frame_in[[var_name]][new_indices_from_in]
      data_frame_out[[var_name]]= replacement_values
      
    }
    
    # define again the light onset
    light_range_index_out = define.whenLightWasOn(data_frame_out)
    light_onset_blue_out = light_range_index_out$blue[1]
    onset_zeroed_time_out = data_frame_out$time - data_frame_out$time[light_onset_blue]
    
    data_frame_out[['time_onsetZero']]= onset_zeroed_time_out
    
    
  } else if (identical(combine_mode, 'trim_to_shortest')) {
    
    data_frame_out = data_frame_in[ind1:ind2,]
  
  }
  
  # You could jitter here, for data augmentation
  # light_range_index_adapt = define.whenLightWasOn(data_frame_out, modeOfOnset = 'adaptive', jitter = jitter)
  # light_onset_blue_adapt = light_range_index_adapt$blue[1]
  
  # Add the max derivative centred time as well
  # t = data_frame_in$time
  # diff_in_seconds = t[light_onset_blue_adapt] - t[light_onset_blue]
  # data_frame_out$time_maxDeriv_zero = data_frame_out$time_onsetZero - diff_in_seconds
  
  ## NORMALIZATION
  if (normalize) {
    data_frame_out = normalize.PLR.reduced(data_frame_out, config_path, 
                                           value_operator = value_operator, normalize_method = normalize_method)
  }
  
  return(data_frame_out)
  
}

get.corresponding.time.indices = function(time_in, time_new, fps, verbose_comb = FALSE) {

  # our frame rate now sets how close the samples can be to each other
  tol = ((1/30) / 2) # and half of the sampling period as the max
  
  # https://stackoverflow.com/questions/49237304/compare-two-vectors-of-numbers-based-on-threshold-of-tolerance-%C2%B1-of-0-5
  boolean_per_time_in = lapply(time_new, function(x) abs(x - time_in) < tol)
  inds = vector(mode='integer', length = length(boolean_per_time_in))
  
  # TODO! vectorized faster version
  for (i in 1 : length(boolean_per_time_in)) {
    
    vector_boolean = boolean_per_time_in[[i]]
    ind = which(vector_boolean == TRUE)
    
    # if this value is not found at all
    if (length(ind) == 0) {
      inds[i] = NA
    
    } else if (length(ind) > 1) { # if duplicate values are found

      # more intuitive conditions
      past_first_ind = (i > 1)
      previous_index_same_as_current_first = inds[i-1] == ind[1]
      
      if (past_first_ind) {
      
        if (is.na(previous_index_same_as_current_first)) {
          previous_index_same_as_current_first = FALSE
        }
        
        if (verbose_comb) {
          cat('i = ', i, '\t ind = ', ind, '\t inds[i-1] =', inds[i-1], 
              # '\t past_first =', past_first_ind, 
               '\t previous_same =', previous_index_same_as_current_first, '\t')
        }
        
        if (previous_index_same_as_current_first) {
          inds[i] = ind[2] # take the second value if the previous was the same as the first
          if (verbose_comb) {
            cat('ind_saved = ', inds[i], '\n')
          }
        } else {
          
          inds[i] = ind[1] # take the first value
          if (verbose_comb) {
            cat('ind_saved = ', inds[i], '\t')
          }
          
          # Do not do for the last value
          if (i < length(boolean_per_time_in)) {
            # Save also for the next i just in case
            inds[i+1] = ind[2] # take the first value
            if (verbose_comb) {
              cat('ind_saved also for next i (', i+1, ') =', inds[i+1], ')\n')
            }
          }
        }
      }
      
    } else {
      inds[i] = ind
    }
  
    # https://stackoverflow.com/questions/41774764/intersection-with-tolerance-of-non-equal-vectors-and-id
    # myDists <- outer(time_in, time_new, FUN=function(x, y) abs(x - y))
    # matches <- which(myDists < tol, arr.ind=TRUE)
    
  }
  
  return(inds)
  
}
  
