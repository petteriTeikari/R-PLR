slope.wrapper.MAIN = function(t, y, y_smooth, uncert_in, rms_error, method) {
  
  # initialize with some value(s)
  significance = NA
  value_index = NA
  offset = NA
  bin_indices = c(1, length(y))
  
  # THE "DUMB" LINEAR FIT
  if (identical(method, 'linearfit') | identical(method, 'linearfit_max')) {
    
    if (identical(method, 'linearfit_max')) {
      index_end = get.adaptive.bin.trimming.indices(t, y, uncert_in, method)
      t = t[1:index_end]
      y = y[1:index_end]
      bin_indices[2] = index_end
      uncert_in = uncert_in[1:index_end]
    }
  
    out_list = linear.fit.wrapper(t, y, uncert_in)
      value = out_list[[1]]
      stdev_slope = out_list[[2]]
      offset = out_list[[3]]
      
    uncert = combine.uncertainties(y, value, stdev_slope,
                                   0, 0, value_index, method) # TODO!
    
      # TODO! Error propagation from input
    
    
    
  # TWO-SEGMENT BROKENSTICK
  } else if (identical(method, 'brokenstick_2_phasic') | 
             identical(method, 'brokenstick_2_sustained') |
             identical(method, 'brokenstick_2_t')) {
    
    # Estimate the breakpoints with the re-smoothed curve, but actually
    # fit the slopes to the raw data
    # TODO! If you want to add a switch later and see what is the difference in robustness
    
    simple.fit = lm(y_smooth~t)
      # simple.fit = rma(y~t, abs(uncert_in), method="FE")
    
    no_of_segm = 2
    z = brokenstick.wrapper(simple.fit, t, y_smooth, data_points_df, uncert_in, method, no_of_segm)

    # If the brokenstick could not fit anything for some reason
    # " Error: at least one coef is NA: breakpoint(s) at the boundary? 
    # (possibly with many x-values replicated)"
    if (is.na(z)) {
      
      value = NA
      uncert = NA
      
    } else {
      
      conf_intervals_for_t = confint(z)
      t_breakpoint = conf_intervals_for_t$t[1]
      SD_for_t = t_breakpoint - conf_intervals_for_t$t[2]
      t_lims = NA
      
      # Return the time point when the stick breaks
      if (identical(method, 'brokenstick_2_t')) {
        
        value = conf_intervals_for_t$t[1]
        uncert = SD_for_t 
        
        # combine.uncertainties(y, value, SD_for_t, 
        #                       rms_error, uncert, 
        #                       value_index, method) # TODO!
        
      } else if (identical(method, 'brokenstick_2_phasic')) {
        
        # TODO! one could dig out this info from "z" out as well
        out = re_fit_brokenstick(t_breakpoint, t, y, uncert_in, 'first_part', t_lims)
        value = out$value
        uncert = out$uncert
        offset = out$offset
        
        index = which.min(abs(t-t_breakpoint))
        bin_indices[2] = index
        
        stdev = 0
        uncert = combine.uncertainties(y, value, stdev, 
                                       rms_error, uncert, 
                                       value_index, method) # TODO!
        
      } else if (identical(method, 'brokenstick_2_sustained')) {
        
        # TODO! one could dig out this info from "z" out as well
        out = re_fit_brokenstick(t_breakpoint, t, y, uncert_in, 'second_part', t_lims)
        value = out$value
        uncert = out$uncert
        offset = out$offset
        
        index = which.min(abs(t-t_breakpoint))
        bin_indices[1] = index
        
        stdev = 0
        uncert = combine.uncertainties(y, value, stdev, 
                                       rms_error, uncert, 
                                       value_index, method) # TODO!
        
      }
    }
    
    
  # THREE-SEGMENT BROKENSTICK
  } else if (identical(method, 'brokenstick_3_phasic') |
             identical(method, 'brokenstick_3_sustained') |
             identical(method, 'brokenstick_3_plateau') |
             identical(method, 'brokenstick_3_t_phasicEnd') |
             identical(method, 'brokenstick_3_t_sustainedEnd')) {
    
    # Estimate the breakpoints with the re-smoothed curve, but actually
    # fit the slopes to the raw data
    # TODO! If you want to add a switch later and see what is the difference in robustness
    
    simple.fit = lm(y_smooth~t)
    no_of_segm = 3
    z = brokenstick.wrapper(simple.fit, t, y_smooth, data_points_df, uncert_in, method, no_of_segm)
    
    # If the brokenstick could not fit anything for some reason
    # " Error: at least one coef is NA: breakpoint(s) at the boundary? (possibly with many x-values replicated)"
    if (is.na(z)) {
      
      value = NA
      uncert = NA
      
    } else {
      
      conf_intervals_for_t = confint(z)
      slopes = slope(z)
      
      # CONTINUE HERE!!!
      
      if (identical(method, 'brokenstick_3_phasic')) {
        
        # First slope
        ind = 1
        value = slopes$t[ind,1]
        uncert = slopes$t[ind,2]
        significance = slopes$t[ind,3]
        
        # NOTE! the slope value above, is the slope using the smoothed y
        # we now compute slope using the "raw" input trace, using the breakpoints
        # given by the brokenstick analysis
        t1 = 0
        t2 = conf_intervals_for_t$t[1,1] # 1st breakpoint
        out = re_fit_brokenstick(0, t, y, uncert_in, 'brokenstick3', c(t1,t2))
        value = out$value
        uncert = out$uncert
        bin_indices = out$bin_indices
        offset = out$offset
        
        
      } else if (identical(method, 'brokenstick_3_sustained')) {
        
        # Second slope
        ind = 2
        value = slopes$t[ind,1]
        uncert = slopes$t[ind,2]
        significance = slopes$t[ind,3]
        
        # NOTE! the slope value above, is the slope using the smoothed y
        # we now compute slope using the "raw" input trace, using the breakpoints
        # given by the brokenstick analysis
        t1 = conf_intervals_for_t$t[1,1] # 1st breakpoint
        t2 = conf_intervals_for_t$t[2,1] # 2nd breakpoint
        out = re_fit_brokenstick(0, t, y, uncert_in, 'brokenstick3', c(t1,t2))
        value = out$value
        uncert = out$uncert
        bin_indices = out$bin_indices
        offset = out$offset
        
      } else if (identical(method, 'brokenstick_3_plateau')) {
        
        # Third slope
        ind = 3
        value = slopes$t[ind,1]
        uncert = slopes$t[ind,2]
        significance = slopes$t[ind,3]
        
        # NOTE! the slope value above, is the slope using the smoothed y
        # we now compute slope using the "raw" input trace, using the breakpoints
        # given by the brokenstick analysis
        t1 = conf_intervals_for_t$t[2,1] # 2nd breakpoint
        t2 = max(t, na.rm=TRUE)
        out = re_fit_brokenstick(0, t, y, uncert_in, 'brokenstick3', c(t1,t2))
        value = out$value
        uncert = out$uncert
        bin_indices = out$bin_indices
        offset = out$offset
        
      } else if (identical(method, 'brokenstick_3_t_phasicEnd')) {
        
        # First breakpoint
        ind = 1
        value = conf_intervals_for_t$t[ind,1]
        ub = conf_intervals_for_t$t[ind,3]
        uncert = ub - value
        
      } else if (identical(method, 'brokenstick_3_t_sustainedEnd')) {
        
        # Second breakpoint
        ind = 2
        value = conf_intervals_for_t$t[ind,1]
        ub = conf_intervals_for_t$t[ind,3]
        uncert = ub - value
        
      }
      
      stdev = 0
      uncert = combine.uncertainties(y, value, stdev, 
                                     rms_error, uncert, 
                                     value_index, method) # TODO!
      
    }
  }
  
  # Return the variables as list
  return(list(value, uncert, significance, bin_indices, offset))
    
}


# SUBFUNCTIONS
re_fit_brokenstick = function(t_breakpoint, t, y, uncert_in, which_part, t_lims) {
  
  diff_t = abs(t_breakpoint - t)
  index_break = which.min(diff_t)
  
  bin_indices = c(1, length(t))
  
  if (identical(which_part, 'first_part')) {
    t_bin = t[1:index_break]
    y_bin = y[1:index_break]
    uncert_bin = uncert_in[1:index_break]
    bin_indices[2] = index_break
      
  } else if (identical(which_part, 'second_part')) {
    t_bin = tail(t, -index_break)
    y_bin = tail(y, -index_break)
    uncert_bin = tail(uncert_in, -index_break)
    bin_indices[1] = index_break
    
  } else if (identical(which_part, 'brokenstick3')) { # for 3-segment case
  
    # get indices corresponding to the bin limits  
    start_ind = which.min(abs(t_lims[1]-t))
    end_ind = which.min(abs(t_lims[2]-t))
      
    t_bin = t[start_ind:end_ind]
    y_bin = y[start_ind:end_ind]
    uncert_bin = uncert_in[start_ind:end_ind]
    
    bin_indices[1] = start_ind
    bin_indices[2] = end_ind
    
  } else {
    warning(which_part, ' not defined for re-fitting for slope, or you had a typo?')
  }
  
  # Ghetto fix when for some reason the vectors are not the same length
  if ( (length(y_bin) != length(t_bin)) |
       (length(y_bin) != length(uncert_bin)) |
       (length(uncert_bin) != length(t_bin))) {
    
      warning('Vectors are not equal length for linear_wrapper() linear regression, using lm()')
    
      # TODO! FIX
      # as problem seem to be at the errors
      out_list = list()
      simple.fit = lm(y~t)
        sumry = summary(simple.fit)
        out_list[[1]] = sumry$coefficients[2]
        out_list[[2]] = 0
      
  } else {
    out_list = linear.fit.wrapper(t_bin, y_bin, uncert_bin)  
  }
  
  # return dataframe
  intercept_se =  out_list[[4]]
  out = data.frame(value = out_list[[1]], uncert = out_list[[2]], 
                   bin_indices = bin_indices, offset =  out_list[[3]])
  
  return(out)
  
}

linear.fit.wrapper = function(t, y, uncert_in) {
  
  # http://www.learnbymarketing.com/tutorials/linear-regression-in-r/
  # Simple regression with no uncertainty
  # simple.fit = lm(y~t)
  # summary(simple.fit)
  # value = simple.fit$coefficients[2] # slope of the fit
  
  # df_plot = data.frame(t=t, y=y, sd=sd)
  # ggplot(df_plot, aes(t)) + 
  #  geom_line(aes(y=y), colour="blue") + 
  #  geom_ribbon(aes(ymin=y-err, ymax=y+err), alpha=0.2)
  
  # https://stats.stackexchange.com/questions/17571/how-to-store-the-standard-errors-with-the-lm-function-in-r
  # stdev_slope = as.numeric(sqrt(diag(vcov(simple.fit)))[2]) # was named numeric, thus as.numeric
  # the_line_fit = simple.fit$fitted.values
  
  # the fit above does not take into account, the uncertainty in input
  # https://stats.stackexchange.com/questions/235693/linear-model-where-the-data-has-uncertainty-using-r
  
  
  no_of_nans = sum(is.na(uncert_in))
  no_of_infs = sum(is.infinite(uncert_in))
  
  length_in = length(uncert_in)
  if (no_of_infs > 0) {
    warning('Infs found from uncertainty vector for linear fit, interpolating them')
    uncert_in[is.infinite(uncert_in)] = NA
    uncert_in <- na.approx(uncert_in)
  }
  length_out = length(uncert_in)
  if (length_out != length_in) {
    options(warn = 1)
    warning('Your uncertainty length changed after Inf interpolation. A bug here!')
    options(warn = -1)
  }
  
  length_in = length(uncert_in)
  if (no_of_nans > 0) {
    
    warning('NA found from uncertainty vector for linear fit, interpolating them')
    # uncert_in[is.na(uncert_in)] = NA
    
    # uncert_in <- na.approx(uncert_in)
    # na.approx will reduce the length for example if the last values are
    # by 4 in other words
    # 54.91287 NA -55.2105 NA NA NA NA
    # 54.91287 -0.1488125 -55.2105
    
    # Let's use this with rule = 2
    # https://stackoverflow.com/questions/7317607/interpolate-na-values-in-a-data-frame-with-na-approx
    uncert_in <- na.approx(uncert_in, rule = 2)
  
  }
  length_out = length(uncert_in)
  
  if (length_out < length_in) {
    options(warn = 1)
    warning('Your uncertainty length changed after NA interpolation. A bug here!')
    options(warn = -1)
  }
  
  # No we cannot have any zeroes in the uncertainty vector, set them to NA, and interpolate
  # the same thing for NAs and Infs
  zero_indices = (0 == uncert_in)
  no_of_zeroes = sum(zero_indices)
  
  length_in = length(uncert_in)
  if (!is.na(no_of_zeroes)) {
    warning('Zeroes found from uncertainty vector for linear fit, interpolating them')
    uncert_in[zero_indices] = NA
    uncert_in <- na.approx(uncert_in)
  }
  length_out = length(uncert_in)
  if (length_out != length_in) {
    options(warn = 1)
    warning('Your uncertainty length changed after zero interpolation. A bug here!')
    options(warn = -1)
  }
  
  stdev_slope = NA
  value = NA

  fit_w_se = rma(y~t, abs(uncert_in), method="FE")
  
  intercept = fit_w_se$b[1]
  intercept_se = fit_w_se$se[1]
  slope = fit_w_se$b[2]
  stdev_slope = fit_w_se$se[2]
  
  value = slope
  
  # library(ggplot2)
  # ggplot(data.frame(t, y), 
  #  aes(x=t, y=y)) + 
  #  geom_point() + 
  #  geom_smooth(method="lm")
  
  return(list(value, stdev_slope, intercept, intercept_se))
  
  
}

# Adaptive BIN
get.adaptive.bin.trimming.indices = function(t, y, uncert_in, method) {
  
  # if needed at some point?
  w = 1 / uncert_in^2
  w = w / max(w)
  
  # you cannot get the minimum value as it is not robust, as there might
  # be a quick constriction followed by oscillation back to bigger pupil sizes
  # and then back to even smaller pupil sizes
  # https://stackoverflow.com/questions/14082525/how-to-calculate-first-derivative-of-time-series
  
  # TODO! at some point
  min_index = which.min(y)
  
  if (identical(method, 'linearfit_max')) {
    min_index = which.min(y)
  } else {
    warning("Typo with method = '", method, "' or you just have not yet defined this for adaptive bin trimming?\n")  
  }
  
  return(min_index)
  
}

brokenstick.wrapper = function(simple.fit, t, y, data_points_df, uncert_in, method, no_of_segm) {
  
  z = NA
  
  if (no_of_segm == 2) {
    
    # https://stackoverflow.com/questions/12135400/errors-in-segmented-package-breakpoints-confusion
    if.false <- F
    no_of_tries = 10
    try = 0
    while(if.false == F & try < no_of_tries){
      
      try = try + 1
      
      tryCatch({
        z = segmented(simple.fit,
                      control=seg.control(display=FALSE))  
        if.false <- T
      }, 
      error = function(e) {
        cat(try, ' ')
      }, 
      finally = {
      })
    }
    
    
  } else if (no_of_segm == 3) {
    
    # Get estimate of the first breakpoint
    z1 = segmented(simple.fit)  
    conf_intervals_for_t = confint(z1)
    t_breakpoint1 = conf_intervals_for_t$t[1]
    
    # Initial guess for the second breakpoint
    # ... between the 1st estimate and the end of "t-vector"
    t_end = max(t)
    t2_guess = (t_end + t_breakpoint1) / 2
    
    # 1 segmented variable, 2 breakpoints: you have to specify starting values (vector) for psi:
    # https://cran.r-project.org/web/packages/segmented/segmented.pdf
    # https://stackoverflow.com/questions/12135400/errors-in-segmented-package-breakpoints-confusion
    if.false <- F
    no_of_tries = 10
    try = 0
    while(if.false == F & try < no_of_tries){
      
      try = try + 1
      
      tryCatch({
        z = segmented(simple.fit, seg.Z=~t, psi=c(t_breakpoint1, t2_guess),
                      control=seg.control(display=FALSE))
        if.false <- T
      }, 
      error = function(e) {
        
        # str(try)
      }, 
      finally = {
        #
      })
    }
  }
  
  if (if.false == FALSE) {
    warning('No fit could be obtained')
    z = NA
  }
  
  return(z)
  
}
