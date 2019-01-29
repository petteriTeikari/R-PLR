ts.filter <- function(df, ts_iter_no, safe_vector, pupil_error) {
  
  # cat("TS Filter (LOESS decomposition) \n")

  # Use the tsoutliers from "forecast" package
  tsout = tsoutliers(df$pupil, ts_iter_no, lambda = NULL)
  
    # For extra background, see:
    # http://freerangestats.info/blog/2016/01/30/hybrid-forecasts
    # https://stats.stackexchange.com/questions/154346/fitted-confidence-intervals-forecast-function-r
    # https://blog.statsbot.co/time-series-anomaly-detection-algorithms-1cef5519aef2
    
  
  # tsoutliers is a new function for the purpose of identifying outliers 
  # and suggesting reasonable replacements. Residuals are identified by 
  # fitting a loess curve for non-seasonal data 
  # and via a periodic STL decomposition for seasonal data.
  
  # indices are now a list of integers
  indices = tsout$index
  
  # make a logical vector with the same length as the input
  indices_logical = logical(length(df$pupil))
  indices_logical[c(indices)] = TRUE
  
  debug_plot = FALSE
  if (debug_plot) {
    ggplot(df, aes(x=df$time)) + 
      geom_point(aes(y=df$pupil/max(df$pupil)), size = 1, alpha = 0.15, colour = "black") +
      geom_point(aes(y=as.numeric(indices_logical)), size = 1, alpha = 0.65, colour = "red") +
      geom_point(aes(y=as.numeric(safe_vector*0.95)), size = 1, alpha = 0.65, colour = "blue") +
      xlab(label="Time [s]") + 
      ylab("Pupil width [px]") 
    sum(indices_logical)
  }  
  
  # Ghetto fix with the safe vector as this method tends to remove sharp edges
  safe_vector = as.logical(safe_vector)
  sum(indices_logical)
  indices_logical = indices_logical & !safe_vector
  sum(indices_logical)
  
  return(indices_logical)
  
}