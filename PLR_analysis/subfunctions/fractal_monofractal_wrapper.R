  fractal.DFA.wrapper = function(t, y, plot_on = FALSE, dfa.package = 'nonlinearTseries') {
    
    # To compute H (the generalized Hurst exponent) that is directly 
    # related to fractal dimension, D, and is a measure of a data series' 
    # "mild" or "wild" randomness.
    
    # A value H in the range 0.5–1 indicates a time series with 
    # long-term positive autocorrelation
    
    # A value in the range 0 – 0.5 indicates a time series with 
    # long-term switching between high and low values in adjacent pairs
    
    # Value of H=0.5 can indicate a completely uncorrelated series
    
    # "Lastly, in recent years, it has been shown that the average wavelet 
    # coefficient method is actually more accurate than the detrended fluctuation analysis"
    # https://doi.org/10.1016%2Fj.jneumeth.2013.10.017
    
    if (identical(dfa.package,'nonlinearTseries')) {
      
      # FROM 'nonlinearTseries' PACKAGE
      # ===============================
      
        # install.packages("nonlinearTseries")
        library(nonlinearTseries)
      
          # IF      #
          # configure: error: missing required header GL/gl.h
          # ERROR: configuration failed for package ‘rgl’
          # THEN
          # sudo apt-get install libglu1-mesa-dev
      
        dfa_nlts = dfa(y, window.size.range = c(50, 300), npoints = 20, do.plot = FALSE)
        H_est = estimate(dfa_nlts, do.plot=FALSE) # 1.742489?
          # TODO! Check if it is correct
        
    } else if (identical(dfa.package,'fractal')) {
      
      # FROM 'FRACTAL' PACKAGE
      # ======================
      
        DFA.walk <- DFA(y, detrend="poly1", sum.order=1)
        
        # the output is now a fractalBlock class object (atomic)
        # attributes(DFA.walk)
        # https://stat.ethz.ch/pipermail/r-help/2008-January/151255.html
        
        fit_coeffs = attributes(DFA.walk)$logfit$coefficients
        fit_intercept = fit_coeffs[1]
        fit_x = fit_coeffs[2]
        
        # Now your fit_x is the H_estimate
        # in other words the Hurst exponent or "Hurst coefficient"
        # https://stackoverflow.com/questions/9433028/hurst-exponent-with-r
        H_est = fit_x
        
        if (plot_on) {
          eda.plot(DFA.walk)          
        }
        
          # compose new signal of detrended oscillations
          # y_wo_base = df_in$loFreq + df_in$hiFreq 
          # DFA.walk_wo_base <- DFA(y_wo_base, detrend="poly1", sum.order=1)
          # print(DFA.walk_wo_base)
          # eda.plot(DFA.walk_wo_base)
    
    }
    
    # Estimate bounds:
    # https://en.wikipedia.org/wiki/Hurst_exponent#Confidence_intervals
    # --> https://doi.org/10.1016%2FS0378-4371%2802%2900961-5
    M = log2(length(y))
    exp_term_lb = -2.93*log(M) + 4.45
    lb = 0.5 - exp(exp_term_lb)
    exp_term_ub = -3.10*log(M) + 4.47
    ub = exp(exp_term_ub) + 0.5
    
    error = NA
    
    # TODO
    # goodness-of-fit, some exponent is always returned independent 
    # of how well the line fits to the data!

    # RETURN
    df_out = data.frame(value = H_est, uncertainty = error, name = 'DFA_Hest',
                        bin_start = 1, bin_end = length(y), offset = NA,
                        stringsAsFactors = FALSE)
    
    return(df_out)
 
    # THEORY (DFA) ------------------------------------------------------------------
    
      # Revisiting detrended fluctuation analysis
      # R. M. Bryce & K. B. Sprague
      # https://www.nature.com/articles/srep00315
      
      # "We show Detrended Fluctuation Analysis introduces artifacts for nonlinear trends, 
      # in contrast to common expectation, and demonstrate that the empirically observed 
      # curvature induced is a serious finite-size effect which will always be present. 
      # Explicit detrending followed by measurement of the diffusional spread of a signals' 
      # associated random walk is preferable, a surprising conclusion given that 
      # Detrended Fluctuation Analysis was crafted specifically to replace this approach. 
      # The implications are simple yet sweeping: there is no compelling reason to apply 
      # Detrended Fluctuation Analysis as it 1) introduces uncontrolled bias; 
      # 2) is computationally more expensive than the unbiased estimator; 
      # and 3) cannot provide generic or useful protection against nonstationaries."
}
