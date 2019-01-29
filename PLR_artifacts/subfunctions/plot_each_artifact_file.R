plot.each.artifact.file <- function(data_frame_in, data_frame_outlierfree, path_out, filename) {
  
  p = ggplot(data_frame_in, 
             aes(x=data_frame_in$time, group=1, ymin=0, ymax=max(data_frame_in$pupil))) + 
              geom_point(aes(y=data_frame_in$pupil), size = 1, color = 'green') + 
              geom_point(aes(y = data_frame_outlierfree$pupil), size = 1, alpha = 0.5, colour = "black") + 
              xlab(label="Time [s]") + 
              ylab("Pupil width [px]") 
  
  print(p)

  # define parts of full filename
  filename_sep = strsplit(filename, .Platform$file.sep)[[1]]
  just_filename = tail(filename_sep, n=1)
  just_path = gsub(just_filename, '', filename)
  
  data_out_plots = file.path(path_out, 'plot', fsep = .Platform$file.sep)
  
  # check if they exist or need to be created
  if (dir.exists(data_out_plots) == FALSE) {
    cat('Creating the subdirectory for clean (outlier-free) PLR data')
    dir.create(data_out_plots, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }
  
  filename_out = sub('.csv', '_plot.png', just_filename)
  ggsave(p, filename = paste(data_out_plots, filename_out, sep = .Platform$file.sep))
  
}