compute.PLR.derivatives = function(t, y, debugON) {
  
  # Smooth first with LOESS
  # https://stackoverflow.com/questions/14082525/how-to-calculate-first-derivative-of-time-series
  # loess.model = loess(y ~ t, span = 0.75)
  # pupil_smooth = predict(loess.model, data.frame(t), se = TRUE)
  # plot(t, pupil_smooth$fit) # too smooth actually
  diff1 = diff(y, differences=1) # 1st derivative
  
  # Pad the derivative to original length
  y_diff1 = vector(mode = 'numeric', length = length(y))
  y_diff1[] = NA
  y_diff1[2:length(y_diff1)] = diff1
  
  # Smooth the 1st derivative 
  # upsample_factor = 4 # with 4 from 30fps to 120fps
  # no_of_samples_in = length(y)
  # time_new = seq(from = t[1], to = tail(t,1), length = upsample_factor*no_of_samples_in)
  
  options(warn = -1)
  loess.model1 = loess(y_diff1 ~ t, span = 0.20)
    # Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,  :
    # Chernobyl! trL>n 22
    # TODO try/catch when you have too many NAs in the y, increase span?
  options(warn = 0)
  
  y_diff1_smooth = predict(loess.model1, data.frame(t), se = TRUE)
  
  # Compute the 2nd derivative as well
  diff2 = diff(y_diff1_smooth$fit, differences=1) # 2nd derivative
  y_diff2 = vector(mode = 'numeric', length = length(y))
  y_diff2[] = NA
  y_diff2[2:length(y_diff2)] = diff2
  
  loess.model2 = loess(y_diff2 ~ t, span = 0.25)
  y_diff2_smooth = predict(loess.model2, data.frame(t), se = TRUE)
  
  # DEBUG PLOT  
  if (debugON) {
    plot(t, y_diff1_smooth$fit)
    points(t, y_diff2_smooth$fit, col='red')
  }
  
  return(list(y_diff1_smooth$fit, y_diff2_smooth$fit))
  
}