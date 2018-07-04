spline.filter <- function(data_frame, sigma_multip, 
                          safe_vector, pupil_error,
                          debug_plot = FALSE, iter = TRUE) {
  
  # accummulate NANs
  nan_indices_out = vector(,length(data_frame$pupil))
  nan_indices_out[] = TRUE
  index_mapping = 1 : length(data_frame$pupil)
  
  # FIRST ITERATION --------------------------------------------------------  
  
    # Inverse of squared error
    # weights_raw = 1 / (pupil_error^2)
    # max_v = max(weights_raw, na.rm = TRUE)
    # weights = weights_raw / max_v
  
    # Quick'n dirty fix as in some cases the final couple of samples are very different 
    # due to small glitch
    data_frame$pupil = quick.n.dirty.glitch.fix(y = data_frame$pupil)    
  
    # Fit a spline 
    fit_out = spline.fit.wrapper(data_frame$time, data_frame$pupil, w = rep(1, length(data_frame$pupil)))
      fit_ts = fit_out[[1]]
      spline_artifacts_noise = fit_out[[2]]
    
    # Check if the output length has changed
    check.the.length.diff(data_frame$pupil, fit_ts$y)
    
    # Compare the fit to allowed distance from detrended mean    
    nan_indices = get_outliers_from_spline_fit(y = data_frame$pupil, 
                                               fit_ts, sigma_multip, safe_vector)
    
    
    if (debug_plot) {
      df_debug = data.frame(x=data_frame$time, y=data_frame$pupil)
      p = list()
      ggplot(df_debug, aes(x=df_debug$x)) + 
                geom_point(aes(y=df_debug$y), size = 1, color = 'green') + 
                geom_point(aes(y=fit_ts$y), size = 1, alpha = 0.25, colour = "black") +
                geom_point(aes(y=50*as.numeric(nan_indices)), size = 1, alpha = 0.25, colour = "red") + 
                xlab(label="Time [s]") + 
                ylab("Pupil width [px]") 
    }  
      
    data_frame_iter = data.frame(time = data_frame$time[!nan_indices])
    data_frame_iter$pupil = data_frame$pupil[!nan_indices]
    nan_indices_iter = nan_indices
    # these indices stay as we need to kick out NAs
    index_mapping = index_mapping[!nan_indices_iter]
    safe_vector_iter = safe_vector[!nan_indices_iter]
    
    if (iter) {
      
      # data_frame_iter = data_frame_iter2
      cat(' .. iterating the spline filter')
    
      fit_out_iter = spline.fit.wrapper(data_frame_iter$time, 
                                   data_frame_iter$pupil,
                                   w = rep(1, length(data_frame_iter$pupil)))
      
      fit_ts_iter = fit_out_iter[[1]]
      safe_vector_iter = safe_vector_iter[!nan_indices_iter]
      safe_vector_iter[] = 0 # do not use this time
      
      # Compare the fit to allowed distance from detrended mean    
      nan_indices_iter = get_outliers_from_spline_fit(data_frame_iter$pupil, 
                                                      fit_ts_iter, 2*sigma_multip, safe_vector_iter)
      
      # update index_mapping
      index_mapping = index_mapping[!nan_indices_iter]
      
      data_frame_iter2 = data.frame(time = data_frame_iter$time[!nan_indices_iter])
      data_frame_iter2$pupil = data_frame_iter$pupil[!nan_indices_iter]
      
    }
  
    if (debug_plot) {
      df_debug = data.frame(x=data_frame_iter$time, y=data_frame_iter$pupil)
      ggplot(df_debug, aes(x=df_debug$x)) + 
        geom_point(aes(y=df_debug$y), size = 1, color = 'green') + 
        geom_point(aes(y=fit_ts_iter$y), size = 1, alpha = 0.25, colour = "black") +
        geom_point(aes(y=50*as.numeric(nan_indices_iter)), size = 1, alpha = 0.25, colour = "red") + 
        xlab(label="Time [s]") + 
        ylab("Pupil width [px]") 
    }  
  
  
  nan_indices_out[index_mapping] = FALSE
  spline_artifacts_noise[nan_indices_out] = NA
    
  return(list(nan_indices, spline_artifacts_noise))
  
}

quick.n.dirty.glitch.fix = function(y) {
  
  indices_last10 = tail(1:length(y),10)
  last10 = y[indices_last10]
  mean_first5 = mean(last10[1:5], na.rm = TRUE)
  SD_first5 = sd(last10[1:5], na.rm = TRUE)
  residuals = last10 - mean_first5
  outliers = abs(residuals) > 1.96*SD_first5
  last10[outliers] = NA
  linfit = lm(last10 ~ indices_last10)
  fit_y = predict(linfit, newdata = data.frame(x=indices_last10))
  
  y_new = fit_y[outliers]
  outlier_indices = indices_last10[outliers]
  
  NA_indices_in = vector(,length(y))
  NA_indices_in[outlier_indices] = TRUE
  
  y_out = y
  y_out[outlier_indices] = y_new
  return(y_out)
  
}

spline.fit.wrapper = function(x, y, w) {
  
  # cat("Cross-validating the correct spline order\n")
  fit_ts = smooth.spline(x=x, y=y, cv=TRUE, w=w) # cross-validate 
  
  # confidence intervals
  # https://stackoverflow.com/questions/23852505/how-to-get-confidence-interval-for-smooth-spline
  res <- (fit_ts$yin - fit_ts$y)/(1-fit_ts$lev) # jackknife residuals
  sigma_scalar <- sqrt(var(res)) # estimate sd
  upper <- fit_ts$y + 1.96*sigma_scalar*sqrt(fit_ts$lev)   # upper 95% conf. band
  lower <- fit_ts$y - 1.96*sigma_scalar*sqrt(fit_ts$lev)   # lower 95% conf. band
  
  # residual error
  ub = abs(fit_ts$y - upper)
  lb = abs(fit_ts$y - lower)
  
  error = ub
  
  # matplot(fit_ts$x, cbind(upper, fit_ts$y, lower, data_frame$pupil), type="plp", pch=".")
  # matplot(spline_artifacts_noise1, type="plp", pch=".")
  
  return(list(fit_ts, error))
  
}


get_outliers_from_spline_fit = function(y, fit_ts, sigma_multip, safe_vector) {
  
  # TODO!
  # STDEV is now global stdev, whereas we could slide a window over the data account for slightly local changes
  
  # check residual, ghetto 1.96*stdev outlier fix
  residual = y - fit_ts$y
  mu = mean(residual) # should be close to zero
  stdev = sd(residual) # "detrended residual"
  
  x = 1:length(residual)
  localmean_loess_of_stdev = loess(residual~x, span = 0.2)
  localmean_of_stdev = localmean_loess_of_stdev$fitted
  
  outlier_SDs_indices = abs(residual) > 1.96*stdev
  cleaned_residual = residual[!outlier_SDs_indices]
  stdev_recompute = sd(cleaned_residual, na.rm = TRUE)
  
  # return the NaN indices (well NA in R)
  nan_indices = abs(residual) > sigma_multip*stdev
  
  # get couple of extreme outliers out
  nan_indices_cleaned = abs(residual) > 2.5*sigma_multip*stdev_recompute
  
  # sum these together
  nan_indices_out = nan_indices_cleaned | nan_indices
  
  # TODO! You could apply Gaussian window to the safe_vector and 
  # flip the values in respect to 1 and multiply the residuals with this?
  
  # Use the safe vector to protect the sharp transitions
  nan_indices_cleaned = nan_indices_cleaned & !as.logical(safe_vector)
  nan_indices_cleaned = as.logical(nan_indices_cleaned)
  
  # sum(nan_indices_cleaned)
  
  return(nan_indices_cleaned)
}


check.the.length.diff = function(y_in, y_out) {

  # PT: Why length changes from 1927 to 1926 for example?
  difference = abs(length(y_in) - length(y_out))
  
  # This is now just for checking whether the lengths match, correct the length before fitting
  if(length(y_in) > length(y_out)) {
    # time = data_frame$time[-length(difference)]
    # pupil = data_frame$pupil[-length(difference)]
    cat("predictor still shorter than data_frame, ", length(y_out), ' vs ', length(y_in), "\n")
  } else if(length(y_in) < length(y_out)) {
    # fit_ts$y = fit_ts$y[-length(difference)]
    # preds$x = preds$x[-length(difference)]
    cat("predictor still longer than data_frame", length(y_out), ' vs ', length(y_in), "\n")
  } else {
    # should be this always, otherwise something fishy is happening
  }
  
}