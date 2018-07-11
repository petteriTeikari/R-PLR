report.visualization = function(filename_path = NA,
                                plot_param = list()) {

  library(ggpubr)
  library(cowplot)
  library(ggplot2)
  theme_set(theme_minimal())
  
  library(dplyr)
  library(reshape2)
  
  library(hht) # Hilbert-Huang
  
  # https://stackoverflow.com/questions/41096293/cowplot-made-ggplot2-theme-disappear-how-to-see-current-ggplot2-theme-and-res
  # https://cran.r-project.org/web/packages/cowplot/vignettes/introduction.html
  
  
  # INIT --------------------------------------------------------------------
  
    source('/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR/plot/retrieve_data_for_visualization.R')
    
    # # Default values if no arguments were given
    # if (is.na(filename_path)) {
    #   filename_path = "/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/reconstructed/PLR1002_reconstruction.csv"  
    # }
    # 
    # if (length(plot_param) == 0) {
    #   plot_param = list()
    #   plot_param[['feats_to_plot']] =  c('MaxConstr', 'QuickPhasic', '6SecondPIPR', 'MFDFA_spectrum_peak_hq')
    # }

  # GET DATA TO PLOT --------------------------------------------------------
  
    cat('Retrieving data for\n')
    cat(filename_path, '\n')
  
    # Get data in plottable format
    data_argout = retrieve.data.for.visualization(filename_path, plot_param)
      path_input = data_argout[[1]]
      plot_param = data_argout[[2]] 
      data_to_plot = data_argout[[3]]


  # FINALLY PLOT A REPORT ---------------------------------------------------

    source('/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR/plot/report_visualization.R')  
    source('/home/petteri/Dropbox/manuscriptDrafts/pupilArtifactsConditioning/PLR_CODE/R-PLR/PLR_IO/define_whenLightWasOn.R')
    
      plot_file.visualization(data_to_plot, plot_param, path_input)
      
      
}



# -----------
# SUBFUNCTIONS -----------------------------------------------------------------------
# -----------


plot_file.visualization = function(data_to_plot, plot_param, path_input) {
  
  # Manual color palette
  plot_param[['color1']][['a']] = 'blue'
  plot_param[['color1']][['b']] = 'red'
  plot_param[['color1']][['c']] = 'black'
  
  # Light periods 
  plot_param[['light_on']] = define.whenLightWasOn(data_to_plot[['recon']])
  
  # For custom layouts 
  # http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
  
  p1 = plot.trace.visualization(data = data_to_plot[['recon']], 
                                x_col = 'time_onsetZero',
                                y_cols = c('pupil_blink_thresholded', 'pupil_outlierfree', 'pupil_outlier_corrected'),
                                title_str = paste0(path_input[['filecode']]), 
                                xlab_str = 'Time', ylab_str = 'Pupil size (normalized)',
                                ylims = c(-80,20),
                                plot_type = 'PLR',
                                plot_param)
  
  p2 = plot.trace.visualization(data = data_to_plot[['recon']], 
                                x_col = 'time_onsetZero',
                                y_cols = c('pupil_outlier_corrected', 'missForest', 'denoised'),
                                title_str = 'Denoised', xlab_str = 'Time', ylab_str = 'Pupil size (normalized)',
                                ylims = c(-80,20),
                                plot_type = 'PLR',
                                plot_param)
  
  ft = plot.feat.visualization(data = data_to_plot[['feats']],
                                plot_type = 'features',
                                stat_path = path_input[['path_feat_stats']],
                                plot_param)
  
  e1 = plot.trace.visualization(data = data_to_plot[['recon']], 
                                x_col = 'time_onsetZero',
                                y_cols = c('base'),
                                title_str = 'Decomposed: BASE', 
                                subtitle_str = NA,
                                ylims = c(-60,30),
                                xlab_str = 'Time', ylab_str = 'Amplitude ',
                                plot_type = 'EMD',
                                plot_param)
  
  e2 = plot.trace.visualization(data = data_to_plot[['recon']], 
                                x_col = 'time_onsetZero',
                                y_cols = c('loFreq'),
                                title_str = 'Decomposed: LOW FREQUENCY', 
                                subtitle_str = NA,
                                ylims = c(-30,30),
                                xlab_str = 'Time', ylab_str = 'Amplitude ',
                                plot_type = 'EMD',
                                plot_param)
  
  e3 = plot.trace.visualization(data = data_to_plot[['recon']], 
                                x_col = 'time_onsetZero',
                                y_cols = c('hiFreq'),
                                title_str = 'Decomposed: HIGH FREQUENCY',
                                xlab_str = 'Time', ylab_str = 'Amplitude ',
                                ylims = c(-10,10),
                                plot_type = 'EMD',
                                plot_param)
  
  tf = plot.tf.visualization(data_tf = data_to_plot[['timefreq']][['hgram']],
                              title_str = 'Time-Frequency | Hilbert Spectrum',
                              subtitle_str = NA,
                              freq.span = c(0, 1.5),
                              plot_param)

  pspec = plot.trace.visualization(data = data_to_plot[['timefreq']][['hspec']], 
                                   x_col = 'frequency',
                                   y_cols = c('amplitude.summed'),
                                   title_str = 'Hilbert Spectrum', 
                                   xlab_str = 'Frequency [Hz]', ylab_str = 'Log(Power)',
                                   data_format = 'list',
                                   plot_type = 'tf_pspec',
                                   xlims = c(0, 1.0),
                                   plot_param)

  fr = plot.trace.visualization(data = data_to_plot[['fractal']][['spec']], 
                                x_col = 'hq',
                                y_cols = c('Dq'),
                                title_str = 'Multifractal spectrum', 
                                xlab_str = 'hq', ylab_str = 'Dq',
                                data_format = 'list',
                                plot_type = 'mf_spectrum',
                                plot_param)
  
  
  # nonlin = ()
  
  # Determine the layout with cowplot
  g = ggdraw() +
    
    # First row, TRACES
    draw_plot(p1, x = 0, y = .66, width = .3, height = .33) +
    draw_plot(p2, x = .3, y = .66, width = .4, height = .33) +
    
    # First row, FEATURES
    draw_plot(ft, x = .7, y = .66, width = .3, height = .33) +
    
    # Second row, EMD
    # https://github.com/haleyjeppson/ggmosaic/issues/9
    # Error in is.finite(x) : default method not implemented for type 'list' 
    # draw_plot(e1, x = 0, y = .44, width = .3, height = .22) +
    # draw_plot(e2, x = 0, y = .22, width = .3, height = .22) +
    # draw_plot(e3, x = 0, y = 0, width = .3, height = .22) +
    
    # Second row, Time-Freq
    draw_plot(tf, x = .3, y = .22, width = .4, height = .44) +
    draw_plot(pspec, x = .3, y = 0, width = .25, height = .22) +
    
    # Second row, Fractal / Nonlinear measures
    draw_plot(fr, x = .7, y = .22, width = .3, height = .44)
    # draw_plot(nonlin, x = .7, y = 0, width = .3, height = .22)
  
  
  filename_out = paste0(path_input[['filecode']], '_results.png')
  fullfile_out = file.path(path_input[['path_output']], filename_out, fsep = .Platform$file.sep)
  cat(' Saving the image as ', filename_out, '\n')
  save_plot(fullfile_out, g, base_height=12)
  
  # ncol = 1, nrow = 1,      base_height=sc*1.8, base_width=sc*3.2)
  
  
}

plot.trace.visualization = function(data, x_col, y_cols, y_col2 = NA,
                                    title_str, subtitle_str = NA,
                                    xlab_str, ylab_str, 
                                    ylims = NA,
                                    xlims = NA,
                                    data_format = 'dataframe',
                                    plot_type = 'PLR',
                                    plot_param,
                                    ratios = NA) {
  
  if (identical(data_format, 'list')) {
    variable_names = names(data)
  } else {
    variable_names = colnames(data)
  }
  
  # select the ones to plot
  x_ind = variable_names %in% x_col
  y_ind = variable_names %in% y_cols
  
  # get the ratio of two columns for visualization purposes
  if (!is.na(y_col2)) {
    y1 = unlist(data[y_ind])
    y_ind2 = variable_names %in% y_col2  
    y2 = unlist(data[y_ind2])
    y_mixed = ratios[1]*y1 + ratios[2]*y2
    df_plot = data.frame(x = data[x_ind], y = data[y_ind])
    df_plot[[y_cols]] = y_mixed
    
  } else {
    # take the subset
    df_plot = data.frame(x = data[x_ind], y = data[y_ind])  
  }
  
  # Rename the columns as "y." was added now
  names(df_plot) <- gsub(x = names(df_plot), pattern = "\\y.", replacement = "")
  
  # Rename just to make things shorter and nicer to look at
  names(df_plot) <- gsub(x = names(df_plot), pattern = "\\pupil_", replacement = "")  
  
  # melt for plot
  df_melt = melt(df_plot, id = x_col)
  colnames(df_melt) = c('Time', 'Block', 'Pupil')
  
  if (identical(plot_type, 'tf_pspec')) {
    df_melt$Pupil = log10(df_melt$Pupil)
  }
  
  p = ggplot(df_melt) +
        geom_line(aes(Time, Pupil, colour = Block)) +
        labs(title = title_str, x = xlab_str, y = ylab_str) +
        # https://ggplot2.tidyverse.org/reference/theme.html
  
  if (identical(plot_type, 'PLR')) {  
    theme(legend.position = "top", legend.text.align = 0, 
          legend.text=element_text(size=rel(0.65))) # not the most adaptive now, TODO!
  } else if (identical(plot_type, 'PLR_video')) {  
    theme(legend.position="none")
  } else {
    theme(legend.position="none")
  }
  
  if (identical(plot_type, 'mf_spectrum')) {
    p = p + geom_point(aes(Time, Pupil), colour = "black", alpha = 0.5)
  }
  
  if (is.na(ylims[1])) {
    # autolimits
  } else {
    p = p + scale_y_continuous(limits = ylims)
  }
  
  if (is.na(xlims[1])) {
    # autolimits
  } else {
    p = p + scale_x_continuous(limits = xlims)
  }
  
  # TODO! Add light periods
  # plot_param[['light_on']]
  # http://is-r.tumblr.com/post/33886259146/adding-a-background-to-your-ggplot ?
  
  # TODO! Horizontal line for the baseline?
  
  # TODO! Custom stylesheet for the typefaces so that it looks more like R-PLR
  # instead of ggplo2?
  
  return(p)
  
}

plot.tf.visualization = function(data_tf, title_str, subtitle_str,
                                 freq.span, plot_param) {
  
  hhg_img = data_tf[['hhg_img']]
  
  tf.2dArray = hhg_img$z
  tf.2dArray = log10(tf.2dArray)
  tf.2dArray[is.infinite(tf.2dArray)] = NA # -Inf -> NA
  
  rownames(tf.2dArray) = hhg_img$x
  colnames(tf.2dArray) = hhg_img$y
  
  z_min = c(min(tf.2dArray, na.rm = TRUE))
  z_max = c(max(tf.2dArray, na.rm = TRUE))
  
  df_melt = melt(tf.2dArray)
  colnames(df_melt) = c('Time', 'Frequency', 'Power')
  
  # http://ggplot2.tidyverse.org/reference/geom_tile.html
  # https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2/
  tf = ggplot(df_melt, aes(Time, Frequency, Power)) +
        geom_raster(aes(fill = Power), interpolate = TRUE) +
        scale_fill_distiller(palette= "Spectral", direction=1) + 
        labs(title = title_str) +
        ylab("Frequency [levels [Hz]") + xlab("Time [s]")
  
  # TODO! use actual units, stored in 
  # time - hhg_img$x
  # frequency - hhg_img$y
  
  # TODO!
  # Add light periods to plot from 
  # "plot_param[['light_on']]"
  # http://is-r.tumblr.com/post/33886259146/adding-a-background-to-your-ggplot ?
  
  # TODO! If you want a bit fancier, you could display the PLR trace 
  # on top of the x-axis and the power spectrum on the y like this:
  # "Scatter plot with marginal density plots"
  # http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
  # http://www.everydayanalytics.ca/2014/09/5-ways-to-do-2d-histograms-in-r.html

  return(tf)
  
}

plot.feat.visualization = function(data, plot_type, stat_path, plot_param) {
  
  df_features = create.feat.df(data, stat_path)
  trim = colnames(df_features) %in% c('mean', 'condition', 'feature') 
  df_trim = df_features[,trim]
  df_melt = melt(df_features[,trim]) 
  
  p = ggplot(df_melt, aes(x = feature, y = value, 
                      colour = condition, fill = condition)) +
          geom_point(size = 3, alpha = 0.7) +
          scale_colour_manual(values = c("deepskyblue3", "black", "darkorange3", "firebrick2"))
  
  p = p + scale_y_continuous(limits = c(-80, 80))
  
  # TODO! if the values are very different, hard to see the differenes for example
  # in fractal measures as they are close to one and then the constrictions can be close 
  # to 60 or something
  
  return(p)
}
  

plot.nonlin.visualization = function() {
  
  library(datasets)
  data(airquality)
  
  p = ggplot(airquality, aes(x = Month, y = Ozone)) + geom_boxplot()
  
  return(p)
  
}
  


create.feat.df = function(data, stat_path) {

  means = read.csv(file.path(stat_path, 'Control_Group_features_mean.csv', fsep = .Platform$file.sep))
  SDs = read.csv(file.path(stat_path, 'Control_Group_features_SD.csv', fsep = .Platform$file.sep))
  no_of_feats_to_plot = length(data)
  
  # create data frame in a loop
  list_loop = list()
  for (f in 1 : no_of_feats_to_plot) {
    feat_name = names(data)[f]
    no_of_conditions = data[[feat_name]][[2]]
    for (c in 1 : no_of_conditions) {
      
      condition_name = names(data[[feat_name]][[1]])[c]
      
      # Get the average values from CONTROL group
      feat_name_in_stat_df = paste0(condition_name, 
                                    '_', feat_name)
      stats_feat_names = colnames(means)
      feat_name_index = which(colnames(means) %in% feat_name_in_stat_df)
      
      if (length(feat_name_index) == 0) {
        if (identical(condition_name, 'diff')) {
          # To implement later
          control_mean = NA
          control_SD = NA
        } else {
          warning('Feature mean and SD not found from CONTROL DF')
          control_mean = NA
          control_SD = NA
        }
      } else {
        control_mean = means[[feat_name_index]]
        control_SD = SDs[[feat_name_index]]
      }
      
      if (c == 1) {
        mean =  data[[feat_name]][[1]][[c]][1]
        sd =  data[[feat_name]][[1]][[c]][2]
        factor = condition_name
        control_stat_mean = control_mean
        control_stat_SD = control_SD
      } else {
        
        mean = c(mean, data[[feat_name]][[1]][[c]][1])
        sd = c(sd, data[[feat_name]][[1]][[c]][2])
        factor = c(factor, condition_name)
        control_stat_mean = c(control_stat_mean, control_mean)
        control_stat_SD = c(control_stat_SD, control_SD)
      }
    }
    
    if (f == 1) {
      list_loop[['mean']] = mean
      list_loop[['SD']] = sd
      list_loop[['condition']] = factor
      list_loop[['feature']] = rep(feat_name, no_of_conditions)
      list_loop[['control_mean']] = control_stat_mean
      list_loop[['control_SD']] = control_stat_SD
      
    } else {
      list_loop[['mean']] = c(list_loop[['mean']], mean)
      list_loop[['SD']] =  c(list_loop[['SD']], sd)
      list_loop[['condition']] =  c(list_loop[['condition']], factor)
      list_loop[['feature']] =  c(list_loop[['feature']], rep(feat_name, no_of_conditions))
      list_loop[['control_mean']] = c(list_loop[['control_mean']], control_stat_mean)
      list_loop[['control_SD']] = c(list_loop[['control_SD']], control_stat_SD)
    }
    
    # list_loop[[feat_name]]
    
  }
  
  df_features = data.frame(list_loop)
  return(df_features)

}