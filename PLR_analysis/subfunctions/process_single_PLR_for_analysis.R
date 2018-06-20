process.single.PLR.for.analysis = function(filename_path, data_path_out, data_norm_path_out, data_images_path_out,
                                           bins, normalize_on, normalize_method, normalize_indiv_colors, 
                                           baseline_period) {
  
  # Separate the fullname to path and filename
  # the IO functions operate now with this separation
  # str(filename)
  if (is.vector(filename_path)) {
    filename_path = unlist(filename_path)
  }
  
  filename_sep = strsplit(filename_path, .Platform$file.sep)[[1]]
  just_filename = tail(filename_sep, n=1)
  just_path = gsub(just_filename, '', filename_path)
  cat(' Analyzing file = ', just_filename, ' ')
  
  # Import the file
  # file_format = "_BR_filt" # The "SERI Format at the moment"
  # data_frame_in = import.pupildata(just_path, filename, file_format)
  
  # Don't really need the "SERI parser" of the past as we are just operating on
  # saved data frames with all the column names there
  data_frame_in = read.csv(filename_path)
  
  # Define when light was on 
  verbose = FALSE
  light_range = define.whenLightWasOn(data_frame_in, verbose)

  # get baseline period from bins
  baseline_period = get.baseline.period.from.bins(bins)
    # TODO! If you want to test the effect of different baseline periods,
    # add some switch from outside to override this
  
  # Normalized here already if you do not have the normalize_indiv_colors as TRUE,
  # i.e. you want to normalize only based on the pre-"light onset" period of the blue light
  # and not invidually both for both colors
  if (normalize_indiv_colors == FALSE) {
    cat('  -- Normalizing the PLR using just the BLUE BASELINE \n')
    color = 'blue'
    data_frame_norm = normalize_PLR(data_frame_in, color, bins, 
                                  normalize_on, normalize_method, normalize_indiv_colors, 
                                  baseline_period, light_range)
    
    # Save this to disk if needed
    export.pupil.dataframe.toDisk(data_frame_norm, filename_path, data_norm_path_out, 'normalized')
    
  } else {
    data_frame__norm = data_frame_in
    
  }
  
  # Do the Computations, i.e. handcrafted features
  out_blue = compute.PLR.features(data_frame_norm, bins, "blue", 
                                       normalize_on, normalize_method, normalize_indiv_colors, 
                                       baseline_period)
  
    features_blue = out_blue[[1]]
    data_bins_blue = out_blue[[2]]
  
  out_red = compute.PLR.features(data_frame_norm, bins, "red", 
                                      normalize_on, normalize_method, normalize_indiv_colors, 
                                      baseline_period)
  
    features_red = out_red[[1]]
    data_bins_red = out_red[[2]]
  
  
  # Plot the results
  plot.PLRwithFeatures(data_frame_norm, bins, features_blue, features_red, 
                         data_bins_blue, data_bins_red,
                         normalize_on, normalize_method, normalize_indiv_colors, 
                         baseline_period, light_range, 
                         just_filename, data_images_path_out)
  
  # Export the results
  export_as = "CSV"
  export.PLRwithFeatures(data_path_out, just_filename, export_as, 
                         data_frame__norm, bins, features_blue, features_red)
  
}


