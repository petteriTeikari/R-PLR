clean.plr <- function(data_frame_in, source_path,  
                      blink_hard_threshold, spline_iter_no, 
                      sigma_multip, debug_clean_on) {

  # Initialization ----------------------------------------------------------
  
    # https://stackoverflow.com/questions/28370249/correct-way-to-specifiy-optional-arguments-in-r-functions
    # if you want to add more arguments
 
    # SOURCE FUNCTIONS
    source(file.path(source_path, 'ts_filter.R', fsep = .Platform$file.sep))
    source(file.path(source_path, 'spline_filter.R', fsep = .Platform$file.sep))
    source(file.path(source_path, 'remove_na.R', fsep = .Platform$file.sep))
    source(file.path(source_path, 'set_outliers_to_na.R', fsep = .Platform$file.sep))
    source(file.path(source_path, 'accumulate_nans.R', fsep = .Platform$file.sep))
    
  
  # Cleaning part -----------------------------------------------------------
  
    pupil_error = data_frame_in$video_noise_total
    verbose_acc = 2; # only the last count
  
    # quick'n'dirty fix for outlier so that limits are more loose on light onsets
    verbose = FALSE
    lights_on = define.whenLightWasOn(data_frame_in, verbose)
    
    padding = 2*10 # frames
    safe_vector = create.safe.vector(data_frame_in$time, lights_on, padding)
    # TODO! Add switch here for multiple ways to kick out outliers
    
    # Remove "illegal values", such as "-1"
  
    # Remove the "clear blinks" using hard threshold
    # indices_hard_thr = data_frame$pupil < blink_hard_threshold
    # cat("Removing obvious blinks with a threshold of", blink_hard_threshold, "px\n")
    indices_hard_thr = data_frame_in$pupil < blink_hard_threshold
    pupil_hard_thr = data_frame_in$pupil
    pupil_hard_thr[indices_hard_thr] = NA # Set to NA the outliers
    
    df_hard_thr = data.frame(time=data_frame_in$time, pupil=pupil_hard_thr) # Put to Dataframe
      df_hard_thr_nonnan = remove.na(df_hard_thr,indices_hard_thr)
      safe_vector_nonnan = safe_vector[!indices_hard_thr]
      pupil_error_nonnan = pupil_error[!indices_hard_thr]
      time_point_list = accumulate.nans(df_hard_thr, indices_hard_thr, verbose_acc)
      
    # Cubic spline smoothing with cross-validated parameters
    # Note! Spline cannot accept any NA values
    spline_out = spline.filter(data_frame = df_hard_thr_nonnan, 
                               sigma_multip, 
                               safe_vector = safe_vector_nonnan, 
                               pupil_error = pupil_error_nonnan,
                               debug_plot = FALSE)
    
      nan_indices_spline = spline_out[[1]]
      spline_error = pupil_error_nonnan
      
      errors_out = make.vector.original.length(df_hard_thr_nonnan$time, spline_error,
                                                                data_frame_in$time)
      
      
      df_spline = set.outliers.to.na(df_hard_thr_nonnan, nan_indices_spline)
      df_spline_nonnan = remove.na(df_hard_thr_nonnan, nan_indices_spline)
      safe_vector_nonnan = safe_vector_nonnan[!nan_indices_spline]
      pupil_error_nonnan = pupil_error_nonnan[!nan_indices_spline]
      time_point_list = accumulate.nans(df_hard_thr_nonnan, nan_indices_spline, verbose_acc, time_point_list)
    
    # Use the tsoutliers from "forecast" package, auto-parameters
    # ts_nan_indices = ts.filter(df_spline_nonnan, 3, safe_vector_nonnan, pupil_error)
      # df_tsout = set.outliers.to.na(df_spline_nonnan, ts_nan_indices)
      # df_tsout_nonnan = remove.na(df_spline_nonnan, ts_nan_indices)
      # safe_vector_nonnan = safe_vector_nonnan[!ts_nan_indices]
      # pupil_error_nonnan = pupil_error_nonnan[!ts_nan_indices]
      # time_point_list = accumulate.nans(df_spline_nonnan, ts_nan_indices, verbose_acc, time_point_list)
   
  
  # OUTPUT ------------------------------------------------------------------

      cat('  - number of outlier points = ', length(time_point_list), '\n')
      data_frame_out = data_frame_in
      
      # after blink thresholding
      data_frame_out$pupil_blink_thresholded = df_hard_thr$pupil
      
      # set actually only now the input to NAs
      final_nan_indices = data_frame_in$time %in% time_point_list
      data_frame_out$pupil_outlierfree = data_frame_in$pupil
      data_frame_out$pupil_outlierfree[final_nan_indices] = NA
      data_frame_out$pupil = data_frame_out$pupil_outlierfree
      
      data_frame_out$safe_vector = safe_vector
      
      data_frame_out$spline_error = errors_out
      data_frame_out$spline_error_additive = sqrt(data_frame_in$video_noise_total^2 +
                                                   data_frame_out$spline_error^2)
      
      data_frame_out$spline_error_fractional = abs(data_frame_out$spline_error_additive /
                                                    data_frame_in$pupil)
      
      return(data_frame_out)
    
}


# SUBFUNCTION FOR LIGHT ONSET/OFFSET protection
create.safe.vector = function(t, lights_on, padding) {

  safe_vector = as.numeric(vector(length=length(t)))
  
  # onset
  safe_vector[(lights_on$red[1]-padding):(lights_on$red[1]+padding)] = 1
  
  # offset
  safe_vector[(lights_on$red[2]-padding):(lights_on$red[2]+padding)] = 1
  
  # onset
  safe_vector[(lights_on$blue[1]-padding):(lights_on$blue[1]+padding)] = 1
  
  # offset
  safe_vector[(lights_on$blue[2]-padding):(lights_on$blue[2]+padding)]  = 1
    
  return(safe_vector)
}

make.vector.original.length = function(error_time, error, time_out) {
  
  # error_time = df_hard_thr_nonnan$time
  # error = spline_error
  # time_out = df_hard_thr$time
  
  # Find corresponding indices
  indices = which(time_out %in% error_time)
  
  errors_out = vector(mode="numeric", length = length(time_out))
  
  # TODO! vectorize
  for (ind in 1 : length(indices)) {
    errors_out[[indices[ind]]] = error[ind]
  }
  
  return(errors_out)
}



