compute.PLR.derivatives = function(t, y, deriv_smooth = 'loess', loess_span = 0.15,
                                   debugON = FALSE, pre_denoising = TRUE) {
  
  # Smooth first with LOESS
  # https://stackoverflow.com/questions/14082525/how-to-calculate-first-derivative-of-time-series
  # loess.model = loess(y ~ t, span = 0.75)
  # pupil_smooth = predict(loess.model, data.frame(t), se = TRUE)
  # plot(t, pupil_smooth$fit) # too smooth actually
  diff1 = diff(y, differences=1, lag=1) # 1st derivative
  
  # Pad the derivative to original length
  y_diff1 = vector(mode = 'numeric', length = length(y))
  y_diff1[] = NA
  y_diff1[2:length(y_diff1)] = diff1
  
  # Smooth the 1st derivative 
  # upsample_factor = 4 # with 4 from 30fps to 120fps
  # no_of_samples_in = length(y)
  # time_new = seq(from = t[1], to = tail(t,1), length = upsample_factor*no_of_samples_in)
  
  if (pre_denoising) {
    
    library(wmtsa)
    library(ifultools)
    library(Rwave)
    
    diff1_denoised = wavShrink(diff1, wavelet='s8',
                               n.level=ilogb(length(diff1), base=2),
                               shrink.fun="hard", thresh.fun="universal", threshold=NULL,
                               thresh.scale=1, xform="modwt", noise.variance=-1.0,
                               reflect=TRUE)
  
    # correct the length
    y_diff1n = vector(mode = 'numeric', length = length(y))
    y_diff1n[] = NA
    y_diff1n[2:length(y_diff1n)] = diff1_denoised
    y_diff1_denoised = y_diff1n
    
    # resid = y_diff1_denoised - y_diff1
    # plot(y_diff1_denoised, type='l')
    # seems useless
  }
  
  options(warn = -1)
  if (identical(deriv_smooth, 'loess')) {
    loess.model1 = loess(y_diff1 ~ t, span = loess_span)
    
      # Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,  :
      # Chernobyl! trL>n 22
      # TODO try/catch when you have too many NAs in the y, increase span?
    
      # Warning messages:
      #   1: In simpleLoess(y, x, w, span, degree = degree, parametric = parametric,  :
      #                       span too small.   fewer data values than degrees of freedom.
      #                     2: In simpleLoess(y, x, w, span, degree = degree, parametric = parametric,  :
      #                                         pseudoinverse used at 15.433
      #                                       3: In simpleLoess(y, x, w, span, degree = degree, parametric = parametric,  :
      #                                                           neighborhood radius 0.0365
      #                                                         4: In simpleLoess(y, x, w, span, degree = degree, parametric = parametric,  :
      #                                                                             reciprocal condition number  0
      #                                                                           5: In simpleLoess(y, x, w, span, degree = degree, parametric = parametric,  :
      #                                                                                               There are other near singularities as well. 0.0021622
    
    options(warn = 0)
    y_diff1_model = predict(loess.model1, data.frame(t), se = TRUE)
    
    if (is.na(y_diff1_model)) {
      span_increment = 0.1
      cat(' initial span (', loess_span,
          ') too small. Increasing it with ', span_increment, 'and try again')
      # TODO real try catch instead of this
      loess.model1 = loess(y_diff1 ~ t, span = loess_span+span_increment)
      y_diff1_model = predict(loess.model1, data.frame(t), se = TRUE)
    }
    
    y_diff1_smooth = y_diff1_model$fit
    
  } else if (identical(deriv_smooth, 'someOtherMethod?')) {

  } else {
    warning('YOUR DIFF SMOOTHING NOT DEFINED YET, A TYPO FOR = ', deriv_smooth, '?')
  }
  
  # Compute the 2nd derivative as well
  diff2 = diff(y_diff1_smooth, differences=1, lag=1) # 2nd derivative
  y_diff2 = vector(mode = 'numeric', length = length(y))
  y_diff2[] = NA
  y_diff2[2:length(y_diff2)] = diff2
  
  if (identical(deriv_smooth, 'loess')) {
    loess.model2 = loess(y_diff2 ~ t, span = loess_span)
    y_diff2_model = predict(loess.model2, data.frame(t), se = TRUE)
    y_diff2_smooth = y_diff2_model$fit
  } else if (identical(deriv_smooth, 'emd')) {
    
  } else if (identical(deriv_smooth, 'ceemd')) {
  
  }
  
  
  # DEBUG PLOT  
  if (debugON) {
    df_plot = data.frame(time = t, signal = y, 
                         velocity_dirty = y_diff1,
                         velocity = y_diff1_smooth,
                         acceleration_dirty = y_diff2,
                         acceleration = y_diff2_smooth)
    
    p = list()
    p[[1]] = ggplot(df_plot, aes(time)) + 
                    geom_line(aes(y = signal, colour = 'signal'))
    
    p[[2]] = ggplot(df_plot, aes(time)) + 
                    geom_line(aes(y = velocity_dirty, colour = 'velocity_dirty')) + 
                    geom_line(aes(y = velocity, colour = 'velocity_loess'))
    
    p[[3]] = ggplot(df_plot, aes(time)) + 
                    geom_line(aes(y = acceleration_dirty, colour = 'acceleration_dirty')) +
                    geom_line(aes(y = acceleration, colour = 'acceleration_loess'))
    
    no_of_cols = 1
    do.call(grid.arrange, c(p, list(ncol=no_of_cols)))
    
  }
  
  return(list(y_diff1_smooth, y_diff2_smooth))
  
}