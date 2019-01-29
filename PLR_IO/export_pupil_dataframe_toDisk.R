export.pupil.dataframe.toDisk <- function(df_in, filename_path, data_path_out, data_type) {
  
  # define parts of full filename
  filename_sep = strsplit(filename_path, .Platform$file.sep)[[1]]
  just_filename = tail(filename_sep, n=1)
  just_path = gsub(just_filename, '', filename_path)
  
  # check if they exist or need to be created
  if (dir.exists(data_path_out) == FALSE) {
    cat('Creating the subdirectory for data export: ', data_path_out, '\n')
    dir.create(data_path_out, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }
  
  # Save the raw in as well (with less columns now)
  fileout = sub('.csv', paste('_', data_type, '.csv', sep = ''), just_filename)
  
  # quick'n'dirty filter for possible double underscore
  fileout = gsub('__', '_', fileout)
  
  fullfileout = file.path(data_path_out, fileout, fsep = .Platform$file.sep)
  cat('\t--> Writing PLR data to "', fileout, '"\n')
  write.csv(df_in, file = fullfileout, row.names=FALSE)
  
  # Save all the data into one HDF5 with all the metadata
  # TODO!
   
  }
  
