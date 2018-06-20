spline.filter <- function(data_frame, sigma_multip, safe_vector, pupil_error) {
  
  # FIRST ITERATION --------------------------------------------------------  
  
    # Inverse of squared error
    weights_raw = 1 / (pupil_error^2)
    max_v = max(weights_raw, na.rm = TRUE)
    weights = weights_raw / max_v
      
    # Fit a spline 
    fit_out = spline.fit.wrapper(data_frame$time, data_frame$pupil, weights)
      fit_ts = fit_out[[1]]
      spline_artifacts_noise = fit_out[[2]]
    
    # Check if the output length has changed
    check.the.length.diff(data_frame$pupil, fit_ts$y)
    
    # Compare the fit to allowed distance from detrended mean    
    nan_indices = get_outliers_from_spline_fit(data_frame$pupil, fit_ts, sigma_multip, safe_vector)
  
      spline_artifacts_noise[!nan_indices] = 0
    
  debug_plot = FALSE
  if (debug_plot) {
    df_debug = data.frame(x=data_frame$time, y=data_frame$pupil)
    ggplot(df_debug, aes(x=df_debug$x)) + 
      geom_point(aes(y=df_debug$y), size = 1, color = 'green') + 
      geom_point(aes(y=fit_ts$y), size = 1, alpha = 0.25, colour = "black") +
      geom_point(aes(y=50*as.numeric(nan_indices)), size = 1, alpha = 0.25, colour = "red") + 
      xlab(label="Time [s]") + 
      ylab("Pupil width [px]") 
  }  
  
  return(list(nan_indices, spline_artifacts_noise))
  
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
  stdev = sqrt(var(residual)) # "detrended residual"
  
  # return the NaN indices (well NA in R)
  nan_indices = abs(residual) > sigma_multip*stdev
  
  # TODO! You could apply Gaussian window to the safe_vector and 
  # flip the values in respect to 1 and multiply the residuals with this?
  
  # Use the safe vector to protect the sharp transitions
  nan_indices = nan_indices & !as.logical(safe_vector)
  nan_indices = as.logical(nan_indices)
  
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