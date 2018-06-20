define.whenLightWasOn = function(data_frame_in, verbose = FALSE, modeOfOnset = 'fixed', jitter = FALSE) {
  
  if (missing(verbose)) {
    verbose = FALSE
  }
  
  if (missing(modeOfOnset)) {
    modeOfOnset = 'fixed'
    # use adaptive if our recordings are not of good quality in terms of the triggering of light onsets
  }
  
  if (missing(jitter)) {
    jitter = FALSE
    # Can be used for data augmentation for deep learning so we shift randonly this and create "novel"
    # synthetic PLR traces
  }
  
  # Determine from R, G, and B vectors
  # "human-friendly def"
  options(warn = -1) # suppress warnings [for green light]
  minR = min(which(data_frame_in$R > 0))
  maxR = max(which(data_frame_in$R > 0))
  minG = min(which(data_frame_in$G > 0))
  maxG = max(which(data_frame_in$G > 0))
  minB = min(which(data_frame_in$B > 0))
  maxB = max(which(data_frame_in$B > 0))
  options(warn = 1) # switch back the warnings
  
  if (is.infinite(maxR)) {
    warning('   No RED light was used for this recording!')
  }

  if (is.infinite(maxB)) {
    warning('   No BLUE light was used for this recording!')
  }
  
  # NOTE! the method, not exactly very clever as we just
  # assume that the blue and lights were on
  
  # Return with the indices
  light_range_index = data.frame(red=c(minR, maxR), 
                                 green=c(minG, maxG), 
                                 blue=c(minB, maxB))
  
  if (identical(modeOfOnset, 'adaptive')) {
    
    # refine the estimate of the onset from the value saved on the logged value
    # Faster to compute and easier to visualize
    margin = 20 # [in samples, around the logged value, empirical "intelligent guess"]
    w1 = minB - margin
    w2 = minB + margin
    
    time_around_onset = data_frame_in$time[w1:w2]
    pupil_around_onset = data_frame_in$pupil[w1:w2]
    
    # plot(time_around_onset, pupil_around_onset)
    
    # Quick and dirty estimate of some sort of onset based on the largest derivative of the signal
    
    # TODO! Now the problem with this approach is that it loses possible delays between pathologies
    # what if in certain condition, the phasic response is normal but severely delayed, so always
    # BETTER TO MAKE SURE THAT YOUR TRIGGER IS DONE PROPERLY FOR THE RECORDING rather than trying
    # to guess after the recording what could be the real ONSET time, 
    
    # or/and read the light onset with
    # a photodiode or something to have an actual feedback that light indeed changed rather than you
    # just writing to the file that the light was on without for example knowing that the electronics 
    # of your device was dying causing problems with the light output?
    
    # Compute 1st and 2nd derivative
    derivatives = compute.PLR.derivatives(time_around_onset, pupil_around_onset, debug=verbose)
      diff1st = derivatives[[1]] # diff2nd = derivatives[[2]]
      max_index = which.max(diff1st)
      max_index_diff = max_index - (margin+1) # difference to logged light onset
      time_of_max_change = time_around_onset[max_index]
      light_onset_time = time_around_onset[(margin+1)]
      
    # Correct the indices now
    light_range_index$red = light_range_index$red + max_index_diff
    light_range_index$green = light_range_index$green + max_index_diff
    light_range_index$blue = light_range_index$blue + max_index_diff
  
  # Now we trust that the times were logged correctly
  } else {
    # NOTHING
  }
  
  if (jitter) {
    # TODO! Later with data augmentation
  }
  
  # Times in seconds as well if you like
  light_range_time = data.frame(red=c(data_frame_in$time[minR], data_frame_in$time[maxR]), 
                                green=c(data_frame_in$time[minG], data_frame_in$time[maxG]), 
                                blue=c(data_frame_in$time[minB], data_frame_in$time[maxB]))
  
  # Display the found times
  if (verbose) {
    cat('RED light was on: ', light_range_time$red[1], '-', light_range_time$red[2], ' seconds \n')
    cat('GREEN light was on: ', light_range_time$green[1], '-', light_range_time$green[2], ' seconds \n')
    cat('BLUE light was on: ', light_range_time$blue[1], '-', light_range_time$blue[2], ' seconds \n')
  }
  
  # return(list(light_range_index, light_range_time))
  return(light_range_index)
  
}