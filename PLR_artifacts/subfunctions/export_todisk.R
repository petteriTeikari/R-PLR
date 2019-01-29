export.todisk <- function(data_path, data_path_out, filename, data_frame_in, data_frame_outlierfree, data_frame_recon) {
  
  # define parts of full filename
  filename_sep = strsplit(filename, .Platform$file.sep)[[1]]
  just_filename = tail(filename_sep, n=1)
  just_path = gsub(just_filename, '', filename)
  
  # TODO! Getting this 
  # "Error in if (file == "") file <- stdout() else if (is.character(file)) { : 
  # argument is of length zero""
  # Problem creating a new file if it does not exist? 
  # problem also when there is something written
  
  # Define output subfolder
  data_out_raw = file.path(data_path_out, 'raw_in', fsep = .Platform$file.sep)
  data_path_out_outlierfree = file.path(data_path_out, 'outlier_free', fsep = .Platform$file.sep)
  data_out_recon = file.path(data_path_out, 'recon', fsep = .Platform$file.sep)
  
  # check if they exist or need to be created
  if (dir.exists(data_out_raw) == FALSE) {
    cat('Creating the subdirectory for clean (outlier-free) PLR data')
    dir.create(data_out_raw, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }
  if (dir.exists(data_path_out_outlierfree) == FALSE) {
    cat('Creating the subdirectory for clean (outlier-free) PLR data')
    dir.create(data_path_out_outlierfree, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }
  if (dir.exists(data_out_recon) == FALSE) {
    cat('Creating the subdirectory for clean (outlier-free) PLR data')
    dir.create(data_out_recon, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }

  # Save the raw in as well (with less columns now)
  fileout = sub('.csv', '_raw.csv', just_filename)
  fullfileout = file.path(data_out_raw, fileout, fsep = .Platform$file.sep)
  cat('Writing Raw PLR data to "', fileout, '"\n')
  write.csv(data_frame_in, file = fullfileout, row.names=FALSE)

  # Now the input dataframes for outlier-free and reconstructed traces
  # have only time and pupil, se we need to copy the other columns as well
  data_frame_outlierfree = copy.inFrameToOutDataframe(data_frame_in, data_frame_outlierfree)
  data_frame_recon = copy.inFrameToOutDataframe(data_frame_in, data_frame_recon)
  
  # TODO! As at the moment no uncertainties are computed for reconstruction, so let's put
  # something there as a placeholder
  # vector(mode = "numeric", length = length(data_frame_recon$time)
  data_frame_recon$confidence_pos = rep(1, length(data_frame_recon$time))
  data_frame_recon$confidence_neg = rep(1, length(data_frame_recon$time))
  
  # Save as .csv the outlier free PLR
  fileout = sub('.csv', '_clean.csv', just_filename)
  fullfileout = file.path(data_path_out_outlierfree, fileout, fsep = .Platform$file.sep)
  cat('Writing outlier free PLR to "', fileout, '"\n')
  write.csv(data_frame_outlierfree, file = fullfileout, row.names=FALSE)
  
  # Save as .csv the reconstructed PLR
  fileout = sub('\\.csv', '_recon.csv', just_filename)
  fullfileout = file.path(data_out_recon, fileout, fsep = .Platform$file.sep)
  cat('Writing reconstructed PLR to', fileout, '\n')
  write.csv(data_frame_recon, file = fullfileout, row.names=FALSE)
 
  # Save all the data into one HDF5 with all the metadata
  # TODO!
   
  }
  