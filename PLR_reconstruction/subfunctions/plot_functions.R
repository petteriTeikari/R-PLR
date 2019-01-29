plot.pair.diff = function(t_ts, y1, y2, title_string, method_string) {
  
  diff = y1 - y2
  
  data_plot <- data.frame(Value = c(y1, y2),
                          Time = t_ts, Method = c('In', method_string))
  
  diff_plot <- data.frame(Value = c(diff),
                          Time = t_ts, Method = c(paste('In - ', method_string, sep='')))
  
  # Compute residual power spectrum
  residual_spec_list  = compute.power.spectrum(diff)
  
  # Compute time-frequency analysis
  in_timeFreq_list = compute.timeFreq(t_ts, y1)
  filt_timeFreq_list = compute.timeFreq(t_ts, y2)
  
  p = list()
  
  p[[1]] = ggplot(data_plot, aes(Time, Value, color = Method)) +
    geom_line(alpha = 0.50, size = 0.8) + 
    labs(title = title_string, subtitle = paste('Filtering with', method_string))
  
  p[[2]] = ggplot(diff_plot, aes(Time, Value, color = Method)) +
    geom_line(alpha = 0.50, size = 0.8) + 
    labs(title = 'Residual Noise')
  
  p[[3]] = ggplot(diff_plot, aes(Value, fill = Method)) +
                  geom_density(alpha = 0.75) +
                  labs(title = 'Residual Distribution') + theme(legend.position='none')
  
    # SORT OF USELESS PLOT
  
      # spec.melt <- melt(residual_spec_list[[1]], id.vars = "freq", value.name = "spec", variable.name = "kernel")
      # p[[3]] = ggplot(spec.melt, aes(freq, spec, color = kernel)) +
      #     geom_line(alpha = 0.50, size = 0.8) +
      #     labs(title = 'Residual Noise Spectrum') +
      #     scale_y_log10("Power Spectrum") + scale_x_continuous("Frequency [Hz]") + theme(legend.position='none')
  
  
  
  # zoom #1
  t1 = -0.5
  t2 = 2.5
  
  p[[4]] = ggplot(data_plot, aes(Time, Value, color = Method)) +
    geom_line(alpha = 0.50, size = 0.8) +
    labs(subtitle = 'Onset zoom') +
    xlim(t1, t2)
    
  p[[5]] = ggplot(diff_plot, aes(Time, Value, color = Method)) +
    geom_line(alpha = 0.50, size = 0.8) +
    xlim(t1, t2) 
  
  modulus = in_timeFreq_list[[2]]
  mod_df = melt(modulus)
  colnames(mod_df) = c('Time', 'Frequency', 'power')
  
  # http://ggplot2.tidyverse.org/reference/geom_tile.html
  p[[6]] = ggplot(mod_df, aes(Time, Frequency, fill = power)) +
    geom_raster(aes(fill = power), interpolate = TRUE)  +
    scale_fill_distiller(palette= "Spectral", direction=1) + # https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2/
    labs(title = 'Input Time-Frequency') +
    ylab("Frequency") + xlab("Time")
  
    
    # CHECK UNITS!
  
  # zoom #2
  t1 = 6
  t2 = 8
  i1 = which.min(abs(t_ts - t1))
  i2 =  which.min(abs(t_ts - t2))
  
  p[[7]] = ggplot(data_plot, aes(Time, Value, color = Method)) +
    geom_line(alpha = 0.50, size = 0.8) +
    xlim(t1, t2) + 
    labs(subtitle = 'Sustained Zoom') +
    ylim(min(c(y1[i1:i2], y2[i1:i2])), max(c(y1[i1:i2], y2[i1:i2])))
    
  p[[8]] = ggplot(diff_plot, aes(Time, Value, color = Method)) +
    geom_line(alpha = 0.50, size = 0.8) +
    xlim(t1, t2)
    
  modulus = filt_timeFreq_list[[2]]
  mod_df = melt(modulus)
  colnames(mod_df) = c('Time', 'Frequency', 'power')
  
  # http://ggplot2.tidyverse.org/reference/geom_tile.html
  p[[9]] = ggplot(mod_df, aes(Time, Frequency, fill = power)) +
            geom_raster(aes(fill = power), interpolate = TRUE)  +
              scale_fill_distiller(palette= "Spectral", direction=1) + # https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2/
              labs(title = 'Filtered Time-Frequency') +
              ylab("Frequency / Scales?") + xlab("Samples")
  
    # CHECK UNITS!
    # ADD LIGHT ONSETS
    
  
  no_of_cols = 3
  do.call(grid.arrange, c(p, list(ncol=no_of_cols)))
  
}