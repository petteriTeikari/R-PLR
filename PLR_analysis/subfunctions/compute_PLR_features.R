compute.PLR.features = function(data_frame_norm, bins, color, 
                                normalize_on, normalize_method, normalize_indiv_colors, 
                                baseline_period) {
  
  bin_names = bins$Name
  bin_methods = bins$Method
  bin_timingMethod = bins$StartString
  
  # Define when light was on
  verbose = FALSE # TODO! now hard-coded
  light_range = define.whenLightWasOn(data_frame_norm, verbose)
  
  # Instead of for-looping, in R, the lapply family is 
  # more efficient, https://nicercode.github.io/guides/repeating-things/
  # feats = lapply(seq_along(bin_names), function(y, n, i) { paste(n[[i]], y[[i]]) }, y=data_frame_norm, n=bins)
  # https://stackoverflow.com/questions/9950144/access-lapply-index-names-inside-fun
  
  # Normalize if desired
  if (normalize_indiv_colors == TRUE) {
    cat('  -- Normalizing the PLR invidually for both colors, now for ', color, '\n')
    data_frame_norm = normalize_PLR(data_frame_norm, color, 
                                normalize_on, normalize_method, normalize_indiv_colors, 
                                baseline_period, light_range)
    
    # Save this to disk if needed
    export.pupil.dataframe.toDisk(data_frame_norm, filename_path, data_norm_path_out, 'normalized_indiv')
  }
    
  # Initialize as list
  features <- list()
  data_bins <- list()
  
  # Cycle through each bin
  options(warn = -1) # suppress warnings [Warning in rma(y ~ t, abs(uncert_in), method = "FE") :
                                         # Studies with NAs omitted from model fitting.]
  for (i in 1:length(bin_names)) {

    # debug
    bin_name = bin_names[i]
    method = bins$Method[i]
    timingMethod = bin_timingMethod[i]
    startTime = bins$Start[i]
    endTime = bins$End[i]
    
    # Get the corresponding points of the bin
    # i.e. Abstractify the rather messy code to determine this
    data_points_df = get.datapoints.per.bin(data_frame_norm, timingMethod, startTime, endTime,
                                            bins$StartString[i], light_range, color)
    
    out = process.all.bins(data_frame_norm, bin_names[i], bin_methods[i], data_points_df,
                                bin_timingMethod[i], bins$Start[i], bins$End[i],
                                bins, i, light_range, color, features, data_bins)
    
    features = out[[1]]
    data_bins = out[[2]]
    
  }
  
  options(warn = 1) # switch back the warnings
  return(list(features, data_bins))
  
} 


# SUBFUNCTION for
# GO THROUGH THE BINS HERE
process.all.bins = function(data_frame_norm, bin_name, method, data_points_df, 
                            timingMethod, startTime, endTime, 
                            bins, i, light_range, color, features, data_bins) {
  
  # Compute then the feature on this subset of the whole PLR
  # depending on the method given for this feature
  feature = compute.indiv.feature(data_points_df, method, bin_name, timingMethod)
  
  # Add this to the previous features computed on previous loop iterations
  # https://stackoverflow.com/questions/17499013/how-do-i-make-a-list-of-data-frames
  features[[i]] = feature
  data_bins[[i]] = data_points_df
  
  return(list(features, data_bins))
  
}


