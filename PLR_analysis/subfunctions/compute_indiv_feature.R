compute.indiv.feature = function(data_points_df, method, bin_name, timingMethod) {

  # return immediately here if the input is invalid
  if (is.na(data_points_df$time[1])) {
    out = data.frame(value = NA, uncert = NA, name = bin_name, stringsAsFactors = FALSE)
    return(out)
    
  } else {
  
    # The input is now a dataframe, that at this point contain
    # $time
    # $pupil (i.e. the PLR trace on this bin)
    # $cerror (95% confidence interval, the positive side)
    # $confidence_neg (95% confidence interval, the negative side)
    
    # Use simpler variables names
    t = data_points_df$time
    y = data_points_df$pupil
    # str(paste('Bin_name = ', bin_name))
    
    # TODO! We simplify now the intervals to be the same on the both side
    err = data_points_df$error
    
    # TODO! Check correct way to define uncertainties for median/min/max later
    uncert_in = err
    rms_error = compute.rms.error(uncert_in)
    
    # STDEV
    stdev = sd(y, na.rm = TRUE)
    n = length(y)
    
    # Index (for single values, otherwise just the NA)
    value_index = NA
    offset = NA # for the slope features
    bin_indices = c(1, length(t))
    
    # Check that everything is correct length
    if ( (length(y) != length(t)) |
         (length(y) != length(uncert_in)) |
         (length(uncert_in) != length(t))) {
      
      warning('Problem with vector lengths')
      str(t)
      str(y)
      str(uncert_in)
      
    }
    
    if (identical(method, 'median')) {
      value = median(y)
      uncert = combine.uncertainties(y, value, stdev, 
                                     rms_error, uncert_in, 
                                     value_index, method)
      
    } else if (identical(method, 'mean')) {
      value = mean(y)
      uncert = combine.uncertainties(y, value, stdev, 
                                     rms_error, uncert_in, 
                                     value_index, method)
      
    } else if (identical(method, 'min')) {
      value = min(y)
      value_index = which.min(y)
      uncert = combine.uncertainties(y, value, stdev, 
                                     rms_error, uncert_in, 
                                     value_index, method)
      
    } else if (identical(method, 'max')) {
      value = max(y)
      value_index = which.max(y)
      uncert = combine.uncertainties(y, value, stdev, 
                                     rms_error, uncert_in, 
                                     value_index, method)
      
    } else if (identical(method, 'max-min')) {
      value = max(y) - min(y)
      value_index = c(which.min(y), which.max(y))
      uncert = combine.uncertainties(y, value, stdev, 
                                     rms_error, uncert_in, 
                                     value_index, method)
       
    } else if (identical(method, 'timing')) {
      
      value_index = which.min(y) 
      value_at_min = y[value_index]
      value = t[value_index] - t[1]
      uncert = 0 # TODO what is the uncertainty now, define small bin?
      uncert = combine.uncertainties(y, value, stdev, 
                                     rms_error, uncert_in, 
                                     value_index, method)
      
    } else if (identical(method, 'linearfit') | identical(method, 'linearfit_max')) {
      
      # TODO! Output slopes as well, and the datapoints so that they can be plotted
      y_smooth = 0
      out_list = slope.wrapper.MAIN(t, y, y_smooth, uncert_in, rms_error, method)
        value = out_list[[1]]
        uncert = out_list[[2]]
        significance = out_list[[3]]
        bin_indices = out_list[[4]]
        offset = out_list[[5]]
      
    } else if (identical(method, 'brokenstick_2_phasic') | 
               identical(method, 'brokenstick_2_sustained') |
               identical(method, 'brokenstick_2_t') |
               identical(method, 'brokenstick_2_phasic') | 
               identical(method, 'brokenstick_2_sustained') |
               identical(method, 'brokenstick_2_t') |
               identical(method, 'brokenstick_3_phasic') |
               identical(method, 'brokenstick_3_sustained') |
               identical(method, 'brokenstick_3_plateau') |
               identical(method, 'brokenstick_3_t_phasicEnd') |
               identical(method, 'brokenstick_3_t_sustainedEnd')) {
      
      # Re-smooth to have more robust estimates
      # smooth_list = re.smooth.trace(t, y, stdev, uncert_in, rms_error, timingMethod)
      #  y_smooth = smooth_list[[1]]
      #  CI = smooth_list[[2]]
      # uncert_in = sqrt(uncert_in^2 + CI ^2) 
      y_smooth = y # TODO! Placeholder now without heavier smoothing  
      
      # TODO! Output slopes as well, and the datapoints so that they can be plotted
      out_list = slope.wrapper.MAIN(t, y, y_smooth, uncert_in, rms_error, method)
        value = out_list[[1]]
        uncert = out_list[[2]]
        significance = out_list[[3]]
        bin_indices = out_list[[4]]
        offset = out_list[[5]]
      
    } else if (identical(method, 'auc')) {
      
      value = trapz(t,y)
      
      # https://math.stackexchange.com/questions/1888244/how-does-uncertainty-of-dataset-propagates-through-numerical-integration
      # auc_uncert = ?
      auc_uncert = 0 # TODO!
      uncert = combine.uncertainties(y, auc, auc_uncert, 
                                     rms_error, uncert_in, 
                                     value_index, method)
    
    } else {
      
      # NOTE! Just good practice to figure out if you have typos or
      # work to do if you add some conditions later on
      warning("Typo with method = '", method, "' or you just have not yet defined this?\n")  
      
    }
    
    # Display if this happens with length more than one
    if (length(value) > 1) {
      # nothing
    }
    
    # Pack everything into a dataframe
    feature = return.feature(value, uncert, bin_name, bin_indices, offset)
    
    # TODO! Significance?
    return(feature)
    
  }
}

return.feature = function(value, uncert, bin_name, bin_indices, offset) {
  
  # essentially a small helper to get rid of duplicates
  # quick'n'dirty
  if ( length(value) > 1 ) {
    value = value[1]
  }
  
  if ( length(uncert) > 1 ) {
    uncert = uncert[1]
  }
  
  if ( length(bin_name) > 1 ) {
    bin_name = bin_name[1]
  }
  
  bin_start = bin_indices[1]
  bin_end = bin_indices[2]
  
  if ( length(offset) > 1 ) {
    offset = offset[1]
  }
  
  feature = data.frame(value = value, 
                       uncertainty = uncert, 
                       name = bin_name, 
                       bin_start = bin_start, 
                       bin_end = bin_end,
                       offset = offset,
                       stringsAsFactors = FALSE)
  
  return(feature)
  
}

re.smooth.trace = function(x, y, stdev, uncert_in, rms_error, timingMethod) {
  
  # https://www.r-bloggers.com/thats-smooth/
  fit.loess = loess(y~x, family="gaussian", span=.20, degree=1)
  
  ## Set a simple 95% CI on the fit.loess model
  ci = cbind(
      predict(fit.loess, data.frame(x=x)),
      predict(fit.loess, data.frame(x=x))+
      predict(fit.loess, data.frame(x=x), se=TRUE)$se.fit*qnorm(1-.05/2),
      predict(fit.loess, data.frame(x=x))-
      predict(fit.loess, data.frame(x=x), se=TRUE)$se.fit*qnorm(1-.05/2)
  )
  
  CI = predict(fit.loess, data.frame(x=x), se=TRUE)$se.fit*qnorm(1-.05/2)
  y_smooth = predict(fit.loess, data.frame(x=x))
  
  # visualize debug
  # plot(x, y, xlim=c(min(x),max(x)), ylim=c(min(y),max(y)), pch=16, cex=.5)
  # matplot(new.x, ci, lty = c(1,2,2), col=c(1,2,2), type = "l", add=T)
  
  return(list(y_smooth, CI))
  
}