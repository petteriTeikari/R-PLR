# Plot Velocity and Acceleration quickly

  # LIBRARIES
  if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
  if (!require("grid")) install.packages("grid"); library("grid")
  if (!require("gridExtra")) install.packages("gridExtra"); library("gridExtra")
  theme_set(theme_light())

  # DATA
  path = '/home/petteri/Dropbox/LABs/SERI/PLR_Folder/DATA_OUT/FinalOUT'
  filename = 'PLR1002_finalData.csv'
  data = read.csv(file.path(path, filename, fsep = .Platform$file.sep))

  # Take only the subset of cols
  # human-readable version of subset
  subject_code = strsplit(filename, '_')[[1]][1]
  time = data$time_onsetZero
  pupil = data$pupil
  velocity = data$velocity
  acceleration = data$acceleration
  blue_on = data$B
  red_on = data$R
  
  # Put back to dataframe to make ggplot plotting easier
  df = data.frame(time = time, pupil = pupil,
                  velocity = velocity, acceleration = acceleration,
                  blue_on = blue_on, red_on = red_on)

  # Plot subfunction
  plot.acc.and.veloc(dataframe = df)
  
  
plot.acc.and.veloc = function(dataframe) {
  
  p = list()
  
  # manual multipliers
  # make dynamic, more intelligent later
  p_multip = c(-60,5)
  v_multip = c(-0.5, 0.5)
  a_multip = c(-0.0055, 0.0055)
  
  # global alpha for the light ON annotations
  alpha_global = .2
  
  # convert the 8-bit intensity to boolean
  blue_ON_boolean = dataframe$blue_on > 0
  red_ON_boolean = dataframe$red_on > 0
  
  # index limits
  blue_ON_ind = c(min(which(blue_ON_boolean)),
                 max(which(blue_ON_boolean)))
  red_ON_ind = c(min(which(red_ON_boolean)),
                 max(which(red_ON_boolean)))
  
  # See help for preparing nice publication quality graph
  # http://thenode.biologists.com/visualizing-data-with-r-ggplot2/education/
  
  # Pupil Size
  p[[1]] = ggplot(data = dataframe, aes(x = time, y = pupil)) +
    geom_line(size=0.75) + 
    labs(title = 'Pupil size', subtitle = 'normalized', x = 'Time [s]', y = 'Norm. size') +
    scale_y_continuous(limits = p_multip) +
    annotate("rect", xmin=time[blue_ON_ind[1]], xmax=time[blue_ON_ind[2]], 
             ymin=-Inf, ymax=Inf, alpha=alpha_global, fill="blue") +
    annotate("rect", xmin=time[red_ON_ind[1]], xmax=time[red_ON_ind[2]], 
             ymin=-Inf, ymax=Inf, alpha=alpha_global, fill="red")
  
  # Velocity
  p[[2]] = ggplot(data = dataframe, aes(x = time, y = velocity)) +
    geom_line(size=0.75) + 
    labs(title = 'Velocity', subtitle = 'from denoised with LOESS smoothing', x = 'Time [s]', y = 'Norm. size / s') +
    scale_y_continuous(limits = v_multip) +
    annotate("rect", xmin=time[blue_ON_ind[1]], xmax=time[blue_ON_ind[2]], 
             ymin=-Inf, ymax=Inf, alpha=alpha_global, fill="blue") +
    annotate("rect", xmin=time[red_ON_ind[1]], xmax=time[red_ON_ind[2]], 
             ymin=-Inf, ymax=Inf, alpha=alpha_global, fill="red")
  
  # Acceleration
  p[[3]] = ggplot(data = dataframe, aes(x = time, y = acceleration)) +
    geom_line(size=0.75) + 
    labs(title = 'Acceleration', subtitle = 'from velocity with LOESS smoothing', x = 'Time [s]', y = bquote('Norm. size / '~s^2))+ 
    scale_y_continuous(limits = a_multip) +
    annotate("rect", xmin=time[blue_ON_ind[1]], xmax=time[blue_ON_ind[2]], 
             ymin=-Inf, ymax=Inf, alpha=alpha_global, fill="blue") +
    annotate("rect", xmin=time[red_ON_ind[1]], xmax=time[red_ON_ind[2]], 
             ymin=-Inf, ymax=Inf, alpha=alpha_global, fill="red")
  
  do.call(grid.arrange, c(p, list(ncol=1, nrow=3)))
  
  ml <- marrangeGrob(p, nrow=3, ncol=1)
  
  # Lo-Res
  ggsave('PLR_demo_loRes.png', ml, , width = 21.59, height = 27.94, units = "cm", dpi = 150)
  
  # Hi-Res
  ggsave('PLR_demo_hiRes.png', ml, , width = 21.59, height = 27.94, units = "cm", dpi = 1200)
  
}


