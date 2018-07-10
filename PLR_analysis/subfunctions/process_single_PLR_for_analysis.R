process.single.PLR.for.analysis = function(filename_path, data_path_out, data_norm_path_out, data_images_path_out,
                                           data_fractal_out, data_timefreq_out, config_path, normalize_on, 
                                           normalize_method, normalize_indiv_colors, 
                                           baseline_period) {
  
  # Separate the fullname to path and filename
  # the IO functions operate now with this separation
  # str(filename)
  if (is.vector(filename_path)) {
    filename_path = unlist(filename_path)
  }
  
  filename_sep = strsplit(filename_path, .Platform$file.sep)[[1]]
  just_filename = tail(filename_sep, n=1)
  filecode = strsplit(just_filename, '_')[[1]][1]
  just_path = gsub(just_filename, '', filename_path)
  cat(' Analyzing file = ', just_filename, ' \n')
  
  # Import the file
  data_frame_in = read.csv(filename_path)
  
  bins = import.binDefinitions(config_path)
  light_range = define.whenLightWasOn(data_frame_in, verbose = FALSE)
  
  # BINS-based FEATURE
  # BLUE
  # ==================
  cat('  Traditional time domain features:')  
  cat(' BLUE ')  
  out_blue = compute.PLR.features(data_frame_in, bins, color="blue", 
                                       normalize_on, normalize_method, normalize_indiv_colors, 
                                       baseline_period)
    features_blue = out_blue[[1]]
    data_bins_blue = out_blue[[2]]
  
  cat('RED\n')  
  out_red = compute.PLR.features(data_frame_in, bins, color="red", 
                                      normalize_on, normalize_method, normalize_indiv_colors, 
                                      baseline_period)
    features_red = out_red[[1]]
    data_bins_red = out_red[[2]]
  
  # Compute Fractal features
  cat('    Fractal features\n')  
  fractal_features = file.fractal.wrapper(filename_path, data_fractal_out, param, debug = FALSE,
                                          dfa.package = 'fractal', from_dataframe = TRUE, 
                                          df_in = data_frame_in, filecode = filecode)
  
  # Compute Time-Frequency analysis
  cat('      Time-Frequency features\n')  
  timefreq_feats = file.timeFreq.wrapper(filename_path, data_timefreq_out, param, debug = FALSE, from_dataframe = TRUE, 
                                            df_in = data_frame_in, filecode = filecode)
  
  timefreq_features = list(data.frame(value = 1,
                            uncertainty = NA,
                            name = 'timefreq_dummy',
                            bin_start = 1, bin_end = 2,
                            offset = NA,
                            stringsAsFactors = FALSE))

  # Plot the results
  # plot.PLRwithFeatures(data_frame_in, bins, features_blue, features_red,
  #                        fractal_features, timefreq_features,
  #                        data_bins_blue, data_bins_red,
  #                        normalize_on, normalize_method, normalize_indiv_colors,
  #                        baseline_period, light_range,
  #                        just_filename, data_images_path_out)
  
  # Export the results
  export_as = "CSV"
  export.PLRwithFeatures(data_path_out, just_filename, export_as, 
                         data_frame__norm, bins, features_blue, features_red,
                         fractal_features, timefreq_features)
  
}


