plot.PLRwithFeatures = function(data_frame_norm, bins, features_blue, features_red,
                                data_bins_blue, data_bins_red, 
                                normalize_on, normalize_method, normalize_indiv_colors, 
                                baseline_period, light_range,
                                just_filename, data_images_path_out) {
  
  # TODO!
  # errors to absolute (just in case)
  data_frame_norm$error = abs(data_frame_norm$error)
  
  no_of_bins = length(bins$Name)
  
  # If you want to plot the light input
  inds_Blue = light_range$blue
  inds_Red = light_range$red
  
  # TODO! check!
  # pupil_raw normalized to one?
  baseline = data_frame_norm$baseline
  signal_in = data_frame_norm$pupil_raw*baseline
  if_pupil_value = TRUE
  data_frame_norm$pupil_raw_norm = normalize.low.level(signal_in, baseline, normalize_method, if_pupil_value)
  
  signal_in = data_frame_norm$pupil_outlierfree*baseline
  if (length(signal_in) == 0) {
    warning('How come there are no values in "signal_in!!')
    # data_frame_norm$pupil_outlierfree does not exist?
  }
  data_frame_norm$outlierfree_norm = normalize.low.level(signal_in, baseline, normalize_method, if_pupil_value)
  
  signal_in = data_frame_norm$pupil_outlier_corrected*baseline
  data_frame_norm$outlier_corrected_norm = normalize.low.level(signal_in, baseline, normalize_method, if_pupil_value)
  
  signal_in = data_frame_norm$pupil_raw*baseline
  data_frame_norm$raw_norm = normalize.low.level(signal_in, baseline, normalize_method, if_pupil_value)
  
  # You can do quick'n'dirty comparison what would happen with LOESS or LOWESS?
  # This is just for visualization now
  span_value = 0.1
  # y.loess <- loess(data_frame_norm$outlierfree_norm ~ data_frame_norm$time, span=span_value)
  # data_frame_norm$pupil_raw_norm_loess = y.loess$fitted
  # TODO! add the NaNs back
  
  # The Plot of the all trace
  # y_lims = c(-60, 20)
  
  # https://stackoverflow.com/questions/33768613/ggplot-legend-not-working-with-scale-colour-manual
  p = ggplot(data_frame_norm, 
             aes(x=data_frame_norm$time, y=data_frame_norm$pupil)) + 
             # geom_line(aes(y=data_frame_norm$raw_norm, colour='Raw'), alpha = 0.4) + 
             geom_line(aes(y=data_frame_norm$outlierfree_norm, colour='Outlier filtered'), alpha = 0.4) + 
             geom_line(aes(y=data_frame_norm$outlier_corrected_norm, colour='Outlier corected'), alpha = 0.9) + 
             geom_line(aes(y=data_frame_norm$pupil, colour='Spline Reconstruction'), alpha = 0.9)
             # geom_line(aes(y=data_frame_norm$pupil_raw_norm_loess, colour=paste('LOESS, span=', span_value)))
             
             # TODO! Some ggplot magic
             # scale_color_discrete(name = "PLR Traces", values = 
             #                        c("Outlier free" = "green", 
             #                          "Reconstruction" = "black", 
             #                          paste('LOESS, span=', span_value)) = "blue")
             # ylim(y_lims)
             
  
    print(p)
    
    # write to disk
    filename_out = sub('.csv', paste('_PLR.png', sep = ''), just_filename)
    path_out = file.path(data_images_path_out, filename_out, sep = .Platform$file.sep)
    path_out = sub('//', '', path_out) # why // at the end?
    path_out_PLR = file.path(path_out, 'PLR', filename_out, sep = .Platform$file.sep)
    ggsave(path_out, p, width = 42.67, height = 24, units = "cm")
  
    
  # BLUE
  combined_bins_for_plot_blue = combine.feats.with.same.bins(data_frame_norm, bins, features_blue, 
                                                        data_bins_blue, normalize_on, normalize_method, 
                                                        normalize_indiv_colors, baseline_period)
  
    # Unwrap the multiple returned out from the list (TODO! if there is a more
    # intelligent way to do this!)
    bins_combined_blue = combined_bins_for_plot_blue[[1]]
    feats_combined_blue = combined_bins_for_plot_blue[[2]]
    
    # Plot these combined features
    plot_bin_features(data_frame_norm, bins, features_blue, data_bins_blue, 
                      bins_combined_blue, feats_combined_blue,
                      normalize_on, normalize_method, 'blue',
                      normalize_indiv_colors, baseline_period, 
                      just_filename, data_images_path_out)
    
  # RED
  combined_bins_for_plot_red = combine.feats.with.same.bins(data_frame_norm, bins, features_red, 
                                                             data_bins_red, normalize_on, normalize_method, 
                                                             normalize_indiv_colors, baseline_period)
  
    # Unwrap the multiple returned out from the list (TODO! if there is a more
    # intelligent way to do this!)
    bins_combined_red = combined_bins_for_plot_red[[1]]
    feats_combined_red = combined_bins_for_plot_red[[2]]
    
    # Plot these combined features
    plot_bin_features(data_frame_norm, bins, features_red, data_bins_red, 
                      bins_combined_red, feats_combined_red,
                      normalize_on, normalize_method, 'red',
                      normalize_indiv_colors, baseline_period, 
                      just_filename, data_images_path_out)
  
  
} # END OF MAIN
  
# COMBINE FEATS with SAME BINS
combine.feats.with.same.bins = function(data_frame_norm, bins, features, 
                                        data_bins, normalize_on, normalize_method, 
                                        normalize_indiv_colors, baseline_period) {

  # Get first the unique data bins based on their time vectors
  bins_combined = get.unique.bins(data_bins)
  
  # No add the features for those bins
  feats_combined = combine.feats.to.unique.bins(bins_combined, data_bins, features)
  
  return(list(bins_combined, feats_combined))
  
}
  
# SUBFUNCTION to GET UNIQUE DATA BINS
get.unique.bins = function(data_bins) {
    
  no_of_bins_in = length(data_bins)
  bins_combined = list()
  unique_bins = 1
  
  # Go through the bins and see how many unique bins we have
  for (b in 1:no_of_bins_in) {
    
    if (length(bins_combined) == 0) { # Always add the first bin
      bins_combined[[b]] = data_bins[[b]] # Assign the trace of this bin
      unique_bins = unique_bins + 1 # Increment the loop index of found unique bins
      
    } else {
      # TODO! maybe there is a vectorized way
      comparison = vector()
      for (uniq in 1:length(bins_combined)) {
        vector1 = bins_combined[[uniq]]$time
        vector2 = data_bins[[b]]$time
        is_equal = (all.equal(vector1, vector2)) 
        comparison = c(comparison, isTRUE(is_equal))
      }
      
      # if not found from found bins, in other words, if a single one
      # of the previous entries is TRUE (sum is more than 1), then that 
      # bin is found, and we do not want to add anything new
      if (sum(comparison) == 0)  {
        bins_combined[[unique_bins]] = data_bins[[b]] # Assign the trace of this bin    
        unique_bins = unique_bins + 1 # Increment the loop index of found unique bins
      }
    }
  }
  
  return(bins_combined)
  
}

# SUBFUNCTION to GET UNIQUE DATA BINS
combine.feats.to.unique.bins = function(bins_combined, data_bins, features) {
  
  unique_bins_in = length(bins_combined)
  feats_combined = vector("list", length = unique_bins_in)
  
  no_of_feats = length(features)
  
  for (feat in 1:no_of_feats) {
    
    time_vector_in = data_bins[[feat]]$time
    feature_name = features[[feat]]$name
    
    # if there is more than one for some reason
    if ( length(feature_name) > 1) {
      feature_name = feature_name[1]
    }
    
    index_to_be_placed_to = NA
    
    # TODO! Vectorize
    for (uniq in 1:unique_bins_in) {
      
      time_vector_in_unique_bins = bins_combined[[uniq]]$time
      
      if (isTRUE(all.equal(time_vector_in, time_vector_in_unique_bins))) {
        index_to_be_placed_to = uniq
        # cat(paste('   ..', feature_name, ', index = ', toString(index_to_be_placed_to)), '\n')
      } 
    }
    
    if (is.na(index_to_be_placed_to)) {
      warning('No index found for = ', feature_name)
    }
    
    feats_combined[[index_to_be_placed_to]][[feature_name]] =
      features[[feat]]
    
  }
  
  return(feats_combined)
  
}

# PLOT
plot_bin_features = function(data_frame_norm, bins, features, data_bins, 
                             bins_combined, feats_combined,
                              normalize_on, normalize_method, color, 
                              normalize_indiv_colors, baseline_period, 
                              just_filename, data_images_path_out) {
  
  no_of_bins = length(bins_combined)
  
  # Init plot list
  p = list()
  
  for (b in 1:no_of_bins) {
    
    df_bin = data.frame(bins_combined[[b]])
    feats = feats_combined[[b]]
    
    p[[b]] =  ggplot(df_bin, 
                     aes(x=time, y=pupil)) + 
                     geom_line(aes(y=pupil), col=color) 
    
    # TODO, annotate if time is from onset or from offset
    no_of_feats_per_bin = length(feats)
    
    feat_string = ''
    for (feat in 1:no_of_feats_per_bin) {
      feat_string = paste(feat_string, 
                          feats[[feat]]$name, ': ',
                          round(feats[[feat]]$value, digits=2), '+/-', 
                          round(feats[[feat]]$uncertainty, digits=2),
                          '\n')
      
      # if time breakpoint
      if (grepl('_t', feats[[feat]]$name) | grepl('Latency', feats[[feat]]$name)) {
        
        color_to_disp ='green'
        if (grepl('_3_t', feats[[feat]]$name)) {
          color_to_disp ='black'
        }
        
        x_ind = which.min(abs(df_bin$time - feats[[feat]]$value))
        # df_bin$time[x_ind]
        y_value = df_bin$pupil[x_ind]
        p[[b]] = p[[b]] + geom_point(x=feats[[feat]]$value,
                                     y=y_value, color=color_to_disp)
        
        # TODO! modify the shape of the point
        # http://sape.inf.usi.ch/quick-reference/ggplot2/shape
        
      # Plot the slope
      } else if (grepl('Slopes', feats[[feat]]$name)) {
        
        color_to_disp ='green'
        if (grepl('_3', feats[[feat]]$name)) {
          color_to_disp ='black'
        }
        
        # geom_abline(intercept = 37, slope = -5)
        # TODO! we don't have the intercept actually stored anywhere
        x1 = df_bin$time[feats[[feat]]$bin_start]
        x2 = df_bin$time[feats[[feat]]$bin_end]
        slope = feats[[feat]]$value
        intercept = feats[[feat]]$offset
        
        y1 = intercept + slope*x1
        y2 = intercept + slope*x2
        
        x = c(x1, x2) 
        y = c(y1, y2)
        
        # p[[b]] = p[[b]] + geom_line(aes(x=x, y=y), col=color_to_disp)
      } 
    }
      
    p[[b]] = p[[b]] +
              annotate("text", x=Inf, y = Inf,
                       label = feat_string,
                       hjust=1,
                       vjust=1,
                       size=3) # TODO! Works for folder save but not well on screen
      
  }
 
  no_of_cols = 3 # TODO! make adaptive to list length
  do.call(grid.arrange, c(p, list(ncol=no_of_cols)))
  
  # https://stackoverflow.com/questions/17059099/saving-grid-arrange-plot-to-file
  
  # https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
  ml <- marrangeGrob(p, nrow=3, ncol=4)
   
  # Save to disk
  filename_out = sub('.csv', paste('_features_', color, '.png', sep = ''), just_filename)
  path_out = file.path(data_images_path_out, filename_out, sep = .Platform$file.sep)
  path_out = sub('//', '', path_out) # why // at the end?
  
  ggsave(path_out, ml, , width = 42.67, height = 24, units = "cm")
  
}
  
 

  # You could try to do a plot with the PLR and a table of the features shown alongisde
  # and save this to disk, so it is easy to browse them through afterwards?
  # e.g. see https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
  
  # OPTIONALLY you could do an additional plot with small subplots of all the different bins 
  # for debuggin the code that you can be more safe that your slopes, AUCs, etc. actually
  # make sense and just don't blindly trust the numbers
  # e.g. see http://r-statistics.co/ggplot2-cheatsheet.html#Multiple%20chart%20panels
  
  # ADVANCED you could make a Dashboard (maybe nicer for the PLR_stats) part to allow some
  # interactive manipulation of parameters and see how this effects your stats (e.g. small
  # tweaks like defining baseline as mean of the bin instead of median, or calculating the
  # baseline using divisive method instead o subtractive, etc.). 
  
  # i.e. try to make the whole
  # software so modular that all these parameters would be given from PLR_toolbox eventually,
  # so that these sort of visualizations could be done along with easier sensitivity analysis
  
