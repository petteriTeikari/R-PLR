data.denoising.wrapper = function(pupil_df, pupil_ts, t, y, error_frac, weights_norm, 
                                  filecodes, param_den, master_data, found_from_master) {
  
  # BACKGROUND and Packages -------------------------------------------------------------
  
    # What is the best method of denoising and smoothing in time series data?
    # https://www.researchgate.net/post/What_is_the_best_method_of_denoising_and_smoothing_in_time_series_data
  
    # TSrepr: Time series representations in R
    # https://cran.r-project.org/web/packages/TSrepr/vignettes/TSrepr_representations_of_time_series.html
  
    # Denoising neural data with state-space smoothing: method and application
    # Hariharan Nalatore, Mingzhou Ding, Govindan Rangarajan
    # https://doi.org/10.1016/j.jneumeth.2009.01.013
  
    # A comparison of denoising methods for one dimensional time series
    # Torsten Kohler, Dirk Lorenz 
    # http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.565.1807&rep=rep1&type=pdf
  
      # The comparison has been carried out within the DFG Priority Program 1114 
      # ”Mathematical methods for time series analysis and digital image processing”.
      # One surprising result is, that in some cases which are assumed to be difficult, 
      # the most easy methods (namely a simple moving average) yield the best results.
  
    # wavelet analyisis to denoising time series
    # http://r.789695.n4.nabble.com/wavelet-analyisis-to-denoising-time-series-td4655006.html
  
  # INPUT CHECK -------------------------------------------------------------
  
    # param_den = param[['denoising']]
  
    # number here refers to differently processed time series objects
    no_of_time_series_in = length(pupil_ts)
    ts_names_in = names(pupil_ts)
    samples_in = dim(pupil_ts[[1]]$pupil)[1]
    observations_in = dim(pupil_ts[[1]]$pupil)[2]
  
  # DENOISING -----------------------------------------------------------------------
    
    no_of_different_denoising_methods = length(param_den[['methods']])
    denoised_list = list()
    
    for (in_data in 1 : no_of_time_series_in) {
    
      for (i in 1 : no_of_different_denoising_methods) {
        
        method_name = param_den[['methods']][i]
        var_name_to_save = paste(method_name, ts_names_in[in_data], sep='=')
        
        cat(var_name_to_save, '\n')
        if (grepl('wmtsa', method_name)) {
          denoised_list[[var_name_to_save]] = denoise.wmtsa.wrapper(t, pupil_ts[[ts_names_in[in_data]]], filecodes, method_name, param_den, debug = FALSE)
          
        } else {
          warning('You have typo for denoising method? "', param_den[['methods']][i], '" not implemented yet')
        }  
      }
    }
    
    return(denoised_list)
    
} 

