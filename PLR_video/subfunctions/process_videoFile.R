process.videoFile = function(data_path, video_data_path_out, filename_path, IO_path) {
  
# IMPORT ------------------------------------------------------------------
  
  # This part now as a placeholder, as we are not using the real video files
  # but we want to have some errors propagated to the artifact removal part
  from_videoResults = TRUE
  
  if (from_videoResults) {
  
    # Define the type of data
    file_format = define.pupillometerFileformat(data_path, filename_path)
    
    # Import data
    data_frame_in = import.pupildata(data_path, filename_path, file_format, IO_path)
    
  } else {
    # if you would be processing the video directly
  }
  
  # Use easier variable names
  t = data_frame_in$time
  y = data_frame_in$pupil

  # NOISE MODELS ------------------------------------------------------------------
  df_pupillometer_noise = define.pupillometerNoiseModel(t, y, data_frame_in, file_format)
  
  # Rename the pupil to pupil_raw and add to the data_frame
  # RATIONALE: we have all the intermediate steps saved, whereas the pipe
  # typically refers to "$pupil" which is always the latest value
  df_pupil_raw = data.frame(pupil_raw = y)
  
  # Add the noise vectors to the input
  df_video_out = cbind(data_frame_in, df_pupil_raw, df_pupillometer_noise)
  
  # EXPORT to DISK
  export.pupil.dataframe.toDisk(df_video_out, filename_path, video_data_path_out, 'video')
  
}

define.pupillometerNoiseModel = function(t, y, data_frame_in, file_format) {
  
  ## ALEATORIC UNCERTAINTY: Inherent to the measurements
  # ----------------------------------------------------
  
  # QUANTIZATION NOISE, i.e. pixels on a sensor are discrete
  quantization_noise = define.pupilQuantizationNoise(t,y)
  
  # PSF Uncertainty, edges are not imaged sharply
  psf_noise = as.numeric(vector(length=length(t)))
  
  # Sensor noise, used ISO, noise in images
  sensor_noise = as.numeric(vector(length=length(t)))
  
  ## EPISTEMIC UNCERTAINTY: Algorithmic uncertainty
  
  # Note that these are not necessarily independent as the algorithm can be rather
  # robust against noise, with sudden breakpoint when the noise becomes too heavy?
  algorithm_noise = as.numeric(vector(length=length(t)))
  
  ## PROPAGATE (i.e. COMBINE) the ERRORS
  noise_total = sqrt(quantization_noise^2 + psf_noise^2 + sensor_noise^2 + 
                     algorithm_noise^2)
  
  ## Return as data_frame
  df_pupillometer_noise = data.frame(error = noise_total,
                                     error_fractional = noise_total / y,
                                     CI_pos = noise_total,
                                     CI_neg = noise_total,
                                     video_quantization_noise = quantization_noise, 
                                     psf_noise = psf_noise,
                                     sensor_noise = sensor_noise,
                                     video_algorithm_noise = algorithm_noise, 
                                     video_noise_total = noise_total,
                                     video_fractional_error = noise_total / y, 
                                     video_CI_pos = noise_total, 
                                     video_CI_neg = noise_total)
  
  return(df_pupillometer_noise)
  
}

define.pupilQuantizationNoise = function(t,y) {
  
  # TODO! Now how come we have non-discrete pupil sizes given that the pupil is defined as the width?
  
  # We assume that the pupil sizes are now defined in pixels, and we assume that we can only detect
  # changes of 0.5 pixels at the most. In other words "physical sizes >0.5 and <1.5 px" would be rounded to 1px
  # WHICH IN TURN means that the relative uncertainty is bigger with smaller pupil sizes
  
  half_pixel_vector = as.numeric(vector(length=length(t)))
  half_pixel_vector[] = 0.5 # set all values to 0.5
                            # https://thomasleeper.com/Rcourse/Tutorials/vectorindexing.html
  
  # TODO! Actually the pupil is width is the square root of the area? that is obtained from fitting an ellipse
  # and calculating the area?
  
  # round input to integers
  y_integer = round(y)
  
  # remove negatives and zeros
  # assume that pupil sizes cannot be negative
  y_integer[y_integer < 1] = 1
  
  # compute the fractional error
  fractional_error = half_pixel_vector / y_integer
  
  # make the fractional error standard deviation of the input pupil sizes
  quantization_noise = fractional_error * y
  
  return(quantization_noise)
  
}


