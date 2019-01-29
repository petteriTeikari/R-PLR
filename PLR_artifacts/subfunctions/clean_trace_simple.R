clean.trace.simple = function(data_frame_in) {
  
  safe_vector = as.numeric(vector(, length(data[i,])))
  
  # filter a bit the incoming trace as especially the noisy ones, are really noise                         )
  df_cpts = changepoint.detection(data_frame_in, method = 'meanvar')
  cpt_indices = is.na(df_cpts$pupil)
  
  sigma_multip = 1.96
  spline_out = spline.filter(data_frame = data_frame_in, 
                             sigma_multip = sigma_multip, 
                             safe_vector = safe_vector, 
                             pupil_error = error,
                             debug_plot = FALSE)
  
  nan_indices_spline = spline_out[[1]]
  no_of_outlier_removed = sum(nan_indices_spline)
  data_frame_in[['pupil']][nan_indices_spline] = NA
  
  # Another pass with LOESS
  trace_cpt = trace_in
  trace_cpt[cpt_indices] = NA
  loess_model = loess(trace_cpt~time, span = 0.05)
  trace_filt = predict(loess_model, time)
  residual = data_frame_in[['pupil']] - trace_filt
  sd_residual = sd(residual, na.rm = TRUE)
  outlier_indices = abs(residual) > sigma_multip*sd_residual
  
  trace_filt2 = data_frame_in[['pupil']]
  trace_filt2[outlier_indices] = NA
  
  plot(time, trace_in, col='red')
  points(time, trace_filt2, type ='l')
  
  
  
  df_out = data.frame(time = data_frame_in$time,
                      pupil = trace_filt2,
                      error = data_frame_in$error)
  
  return(df_out)
  
}