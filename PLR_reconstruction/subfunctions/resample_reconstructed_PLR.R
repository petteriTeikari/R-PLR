resample.reconstructed.PLR = function(data_path_out, data_resampled_path_out, data_trimmed_path_out, 
                                      filename_path, fps, time_lims, time_lims_in, dexclusion_indices) {
  
  # str(filename_path)
  data_frame_in = read.csv(filename_path)
  
  # WHEN WAS LIGHT ON
  ONSET_frame = define.whenLightWasOn(data_frame_in, verbose = FALSE)
  blue_onset_ind = ONSET_frame$blue[1]
  
  # time_lims_ind = c(time_limits[5], time_limits[6])
  
  # Define the new time vector
  min_time = time_lims[1]
  max_time = time_lims[2]
  time_range = max_time - min_time
  new_length = floor(time_range*fps)
  t_new = seq(from=1/fps, to=min_time_ceil, length.out = new_length)
  
  col_names = colnames(data_frame_in)
  
  vars_to_excl = c('frame')
  to_keep = is.na(match(col_names, vars_to_excl))
  vars_to_resample = col_names[to_keep]
  
  list_out = list()
  list_out[['frame']] = seq(from=1, to=new_length, length.out = new_length)
  t_in = data_frame_in$time
  
  trim_out = list()
  trimmed_length = sum(time_lims_ind)+1 # add one to end to include the end
  trim_out[['frame']] = seq(from=1, to=trimmed_length, length.out = trimmed_length)
  
  # the trimming indices from 
  
  for (i in 1:length(vars_to_resample)) {
    
    var_name = vars_to_resample[i]
    y_in = data_frame_in[[var_name]]
    
    # RESAMPLING
    
      # Define the fitting model (spline model)
      # RESAMPLE using a SPLINE INTERPOLATION
      # TODO! Deep learning based of course at some point
      func = splinefun(x=t_in, y=y_in, method="fmm",  ties = mean)
        
      # Apply the defined model for your newly created time vector
      y_new = func(t_new)
      list_out[[var_name]] = y_new
      
    # TRIM
      
      ind1 = blue_onset_ind - time_lims_ind[1]
      ind2 = blue_onset_ind + time_lims_ind[2]
      trim_out[[var_name]] = y_in[ind1:ind2]
    
  }
  
  df_out = data.frame(list_out)
  df_trim_out = data.frame(trim_out)
  # plot(df_out$time, df_out$pupil)
  
  # Save to disk
  export.pupil.dataframe.toDisk(df_out, filename_path, data_resampled_path_out, 'recon_resampled')
  
  # Save to disk
  export.pupil.dataframe.toDisk(df_trim_out, filename_path, data_trimmed_path_out, 'recon_trim')
  
  
  
}