get.timing.stats.of.inputs = function(data_path, data_path_out, 
                                      IO_path, 
                                      filename_path, param, 
                                      time_lims) {

  data_frame_in = read.csv(filename_path)
  
  # debug
  filename_sep = strsplit(filename_path, .Platform$file.sep)[[1]]
  just_filename = tail(filename_sep, n=1)
  subject_code = strsplit(just_filename, '_')[[1]][1]
  just_path = gsub(just_filename, '', filename_path)
  cat('Reading in = ', subject_code, ' \n')
  
  # correct the time for light onset
  ONSET_frame = define.whenLightWasOn(data_frame_in, verbose = FALSE)
  blue_onset_ind = ONSET_frame$blue[1]
  onset_time = data_frame_in$time[blue_onset_ind]
  data_frame_in$time_onsetZero = data_frame_in$time - onset_time
  # TODO! do this already in the VIDEO part
  
  # We could check now how many samples do we have between the start index hard-coded
  # defined in the "time_lims"
  start_ind = which.min(abs(data_frame_in$time_onsetZero - time_lims[1]))
  end_ind = which.min(abs(data_frame_in$time_onsetZero - time_lims[2]))
  
  samples_before_onset = blue_onset_ind - start_ind # Not including the start_ind
  samples_after_onset = end_ind - blue_onset_ind
  samples_after_onset = samples_after_onset + 1 # include the onset sample
  
  # Plot
  plot_output = FALSE
  if(plot_output == TRUE) {
    # Takes the most time in this processing actually, even with a fast HDD
    plot.eachfile(data_frame_in, data_frame_inlierfree, data_frame_recon, path_out, filename)
  }

  # 
  time_limits_df = data.frame(min_time_in = min(data_frame_in$time), 
                              max_time_in = max(data_frame_in$time),
                              min_time_onsetZero = min(data_frame_in$time_onsetZero),
                              max_time_onsetZero = max(data_frame_in$time_onsetZero),
                              samples_before_onset = samples_before_onset,
                              samples_after_onset = samples_after_onset)
  
  time_limits = c(time_limits_df$min_time_in, time_limits_df$max_time_in,
                  time_limits_df$min_time_onsetZero, time_limits_df$max_time_onsetZero,
                  samples_before_onset, samples_after_onset)
  
  return(time_limits)
  
}

reconstruct.PLR.file = function(data_frame_inlierfree, method_recon) {

  # Placeholder when waiting for something more sophisticated
  nan_indices = is.na(data_frame_inlierfree$pupil)
  
  # TODO!
  # GHETTO FIX IF FIRST OR LAST VALUE IS NAN
  
  df_nonnan = remove.na.from.whole.df(data_frame_inlierfree, nan_indices)
  
  # Comparison of different Methods for Univariate Time Series Imputation in R
  # https://arxiv.org/abs/1510.03924
  
  # Impute with smoothing spline
  # http://blog.revolutionanalytics.com/2015/09/interpolation-and-smoothing-functions-in-base-r.html
  fit_ts = smooth.spline(x=df_nonnan$time,  y=df_nonnan$pupil, cv=TRUE) # cross-validate 
  
  # use the original time vector to interpolate the data with the spline model
  preds <- predict(fit_ts, x=data_frame_inlierfree$time) 
  
  recon_noise = vector('numeric', length=length(data_frame_inlierfree$time))
  recon_noise_additive = sqrt(recon_noise^2 + data_frame_inlierfree$spline_error_additive^2)
  
  data_frame_in = data.frame(pupil = preds$y)
  
  return(data_frame_in)
  
}

remove.na.from.whole.df = function(df, nan_indices) {
  
  col_names = colnames(df)
  df_as_list = list()
  
  for (i in 1:length(col_names)) {
    colname = col_names[i]
    vector_nonnan = df[[colname]][!nan_indices]
    df_as_list[[colname]] = vector_nonnan
  }
  
  # back to df
  df_out = data.frame(df_as_list)
  return(df_out)
  
}
