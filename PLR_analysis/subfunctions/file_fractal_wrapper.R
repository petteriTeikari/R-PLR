file.fractal.wrapper = function(filename_path, data_fractal_out, param, debug = FALSE,
                                dfa.package = 'fractal', from_dataframe = FALSE, 
                                df_in = NA, filecode = NA) {

  # INITIALIZE --------------------------------------------------------------

    if (!from_dataframe) {
      
      # Check input
      just_filename = tail(strsplit(filename_path, .Platform$file.sep)[[1]], 1)
      filecode = strsplit(just_filename, '_')[[1]][1]
      cat(filecode, ' ')
      
      # READ IN
      df_in = read.csv(filename_path)
    
    } else {
      
      # df_in given as input
      
    }
  
  # COMPUTATIONS ------------------------------------------------------------

    t = df_in$time
    y = df_in$pupil
    
    if (sum(is.na(y))) {
      warning('Your input "y" has NA values!, More exactly = ', 
              100*sum(is.na(y))/length(y), '% of values are NA')
    }
    
    # DFA, not the most useful in general
    # 'rgl' library is so tricky
    DFA_df = list(fractal.DFA.wrapper(t, y))
    
    # MMA, Multiscale Multifractal Analysis
    # TODO! at some point
    
    # MFDFA, MultiFractal Detrended Fluctuation Analysis 
    mfdfa_out = fractal.MFDFA.wrapper(t, y)
      mfdfa = mfdfa_out[[1]] # the vectors that are saved to disk
      mfdfa_list_of_dfs = mfdfa_out[[2]] # point estimates (scalars) that can be analyzed with the other
                                # classical features
    
    file_out = paste0(filecode, '_mfdfa.RData')
    save(mfdfa, file = file.path(data_fractal_out, file_out, fsep = .Platform$file.sep))

    scalars_out = c(DFA_df, mfdfa_list_of_dfs)
    return(scalars_out)
    
  # MULTIFRACTAL THEORY ------------------------------------------------------------------
    
    # Multiscale multifractal analysis of heart rate variability recordings 
    # with a large number of occurrences of arrhythmia
    # J. Gierałtowski, J. J. Żebrowski, and R. Baranowski
    # Phys. Rev. E 85, 021915
    # https://doi.org/10.1103/PhysRevE.85.021915
    
        # We introduce a method called multiscale multifractal analysis (MMA), 
        # which allows us to extend the description of heart rate variability to 
        # include the dependence on the magnitude of the variability and time scale 
        # (or frequency band). MMA is relatively immune to additive noise and nonstationarity, 
        # including the nonstationarity due to inclusions into the time series of events of a 
        # different dynamics (e.g., arrhythmic events in sinus rhythm). 
    
    # Characterizing scaling properties of complex signals with 
    # missed data segments using the multifractal analysis
    # Chaos 28, 013124 (2018); https://doi.org/10.1063/1.5009438
    
        # The scaling properties of complex processes may be highly influenced by the presence of 
        # various artifacts in experimental recordings. Their removal produces changes in the 
        # singularity spectra and the Hölder exponents as compared with the original artifacts-free data, 
        # and these changes are significantly different for positively correlated and anti-correlated signals.
        # ..
        # We show that even an extreme data loss allows characterizing physiological 
        # processes such as the cerebral blood flow dynamics.
    
    
}
