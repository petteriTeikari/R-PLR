decomp.EMD.wrapper = function(t, data_in, filecodes, method_name, param_decomp, debug) {

  no_of_observ = dim(t)[2] # e.g 10 subjects
  no_of_samples_per_observ = dim(t)[1] # e.g. 30 sec * 30 fps = 900 frames [i.e. samples]
  
  list_of_EMD_lists = list()
  
  cat('Running an EMD for the PLR Data (subject-by-subject) using the variant =', method_name, '\n')
  
  # TODO! Parallelize this
  for (i in 1 : no_of_observ) {
    EMD_list = decomp.EMD.per.subject(t[,i], data_in[,i], filecodes, method_name, param_decomp, debug)
    list_of_EMD_lists = c(list_of_EMD_lists, EMD_list)
  }
    
}

decomp.EMD.per.subject = function(t_t, sig, filecodes, method_name, param_decomp, debug) {
  
  model <- loess(sig ~ t_t, span = 0.1) # estimate the local mean with LOESS
  noise.amp <- sd(model$residuals)
  trials <- as.numeric(param_decomp[['EMD_trials']])
  nimf = 10
  
  # EMD reasonably fast
  # CCEEMD slow as hell
  
  start_time <- Sys.time()
  if (identical(method_name, 'EMD')) {
    emd.result <- Sig2IMF(sig, t_t, sm = "polynomial", max.imf = 10) # ~1.83 secs
  } else if (identical(method_name, 'CEEMD')) { 
    emd.result <- CEEMD(sig, t_t, noise.amp, trials, verbose = TRUE) # ~2.84 mins
    # Error in NextMethod(.Generic) : cannot assign 'tsp' to zero-length vector

  } else {
    warning('You tried EMD variant = "', method_name, '" which is not implemented yet, or was this a typo?')
  }
  end_time <- Sys.time()
  end_time - start_time
  
  return(emd.result)
  
}



                                  

EMD.demo = function() {
  
  i = 369
  tt = t[,i]
  sig = y[,i]
  model <- loess(sig ~ tt, span = 0.1) # estimate the signal mean with LOESS
  
  ## CEEMD
  
    # https://cran.r-project.org/web/packages/hht/hht.pdf
    # pg .4
    noise.amp <- sd(model$residuals)
    trials <- 100
    
    # https://www.rdocumentation.org/packages/hht/versions/2.1.3/topics/CEEMD
    # Keep in mind that the CEEMD is a computationally expensive algorithm and may take significant time to run.
    start_time <- Sys.time()
    ceemd.result <- CEEMD(sig, tt, noise.amp, trials, verbose = FALSE)
    end_time <- Sys.time()
    end_time - start_time # Time difference of 2.838 mins
    
    # https://www.rdocumentation.org/packages/hht/versions/2.1.3/topics/PlotIMFs
    par(mar=c(1,1,1,1)) # https://stackoverflow.com/questions/23050928/error-in-plot-new-figure-margins-too-large-scatter-plot
    PlotIMFs(ceemd.result, imf.list = 1:10)
    
    imf.sum.list = 2:10
    imfs_trimmed = ceemd.result$imf[,imf.sum.list]
    imf_filter = rowSums(imfs_trimmed) # IMFs
    imfs_with_residue = imf_filter + ceemd.result$residue # add the trend also back
    
    title_string = paste(filecodes[i], 'Kept IMFs from', imf.sum.list[1], 'to', tail(imf.sum.list,1))
    plot.pair.diff(tt, sig, imfs_with_residue, title_string, 'CEEMD')
    
  ## EEMD
    
    nimf = 10
    start_time <- Sys.time()
    eemd.result <- EEMD(sig, tt, noise.amp, trials, nimf, verbose = TRUE)
    end_time <- Sys.time()
    end_time - start_time # Time difference of 1.075779 mins
    
    # https://www.rdocumentation.org/packages/hht/versions/2.1.3/topics/PlotIMFs
    PlotIMFs(eemd.result)
    
  ## EMD
    
    start_time <- Sys.time()
    emd.result <- Sig2IMF(sig, tt, sm = "polynomial")
    end_time <- Sys.time()
    end_time - start_time # Time difference of 1.825828 secs
    
    imf.sum.list = 6:10
    imfs_trimmed = emd.result$imf[,imf.sum.list]
    imf_filter = rowSums(imfs_trimmed) # IMFs
    imfs_with_residue = imf_filter + ceemd.result$residue # add the trend also back
    
    title_string = paste(filecodes[i], 'Kept IMFs from', imf.sum.list[1], 'to', tail(imf.sum.list,1))
    plot.pair.diff(tt, sig, imfs_with_residue, title_string, 'EMD')
    
    PlotIMFs(emd.result)
    
  
}
  