compute.power.spectrum = function(y, f_s = 30, kernel_name= "daniell", k_size = c(9)) {
  
  # Compute power spectrum of the diff signal, in other words the power spectrum of the 
  # residual removed from the input by the denoising for example
  
  # https://rstudio-pubs-static.s3.amazonaws.com/9428_1197bd003ebd43c49b429f22ea4f36e5.html
  k = kernel(kernel_name, k_size)
  smooth.spec <- spec.pgram(y, kernel = k, taper = 0, pad=TRUE, plot = FALSE)
  
  # correct the normalized frequency with the real sampling rate f_s
  smooth.spec$freq = smooth.spec$freq * f_s
  
  # convert into data frame
  spec_name = paste(kernel_name, '_c(', k_size, ')', sep='')
  spec.df <- data.frame(freq = smooth.spec$freq, smoothed_spec = smooth.spec$spec)
  
  # return bandwidth also
  bw = smooth.spec$bandwidth * f_s
  
  return(list(spec.df, bw))
}
  